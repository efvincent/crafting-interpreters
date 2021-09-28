module Flox.Parser

// Suppress a warning relating to the initialization soundness of recursively defined bindings
// that are created using the `makeRule` function below
#nowarn "40"  

open Flox.Tokens
open Flox.Expressions
open Flox.Statements
open Flox.ErrorHandling

(*
grammar for flox

program     -> declaration* EOF ;

declaration -> varDecl 
              | statement;

varDecl     -> "var" IDENTIFIER ( "=" expression )? ";" ;

statement   -> exprStmt 
              | printStmt ;

exprStmt    -> expression ";" ;

printStmt   -> "print" expression ";" ;

expression  -> assignment ;

assignment  -> IDENTIFIER "=" assignment | equality;

equality    -> comparison ( ( "!=" | "==") comparison )* ;

comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term        -> factor ( ( "-" | "+" ) factor )* ;

factor      -> unary ( ( "/" | "*" ) unary )* ;

unary       -> ( "!" | "-" ) unary 
              | primary ;

primary     -> NUMBER | STRING 
              | "true" | "false" | "nil" 
              | "(" expression ")"
              | IDENTIFIER ;

*)

let private makeExprRule nextRule matchTypes =
  fun tokens ->
    let rec loop result =
      result
      |> Result.bind (fun (expr, rest) ->
          match rest with 
          | (t::rest') when List.contains t.Type matchTypes ->
            nextRule rest'
            |> Result.map  (fun (right, rest'') -> Binary (expr, t, right), rest'' ) 
            |> loop
          | _ -> Ok (expr, rest)
        )
    nextRule tokens
    |> loop

/// The top level expression rule, all expression variants are cases of this rule. It is
/// an alias of the assignment rule
let rec private expression = assignment

/// assignment  -> IDENTIFIER "=" assignment | equality;
/// If we have an identifier token followed by the equals token, we evaluate the right side
/// with the assignment rule, otherwise we pass the tokens to the equality rule
and private assignment tokens =
  result {
    // evaluate an equality, which would consume up to, but not including the equals
    match! equality tokens with
    | (Var name, equals::rest) when equals.Type = EQUAL -> 
      // in the case where we have a variable and an equals, the "rest" should have an expression
      // for the right hand side of the assignment. Get that rhs, build and return the assignment
      let! (rhs, tkns') = assignment rest
      return (Assignment (name, rhs), tkns')
    | (_, equals::rest) when equals.Type = EQUAL ->
      // in the case where we have a something (not a variable) and an equals sign, that's an
      // invalid assignment target
      return! Error <| (FloxError.FromToken (List.head tokens) "Invalid Assignment Target", rest)
    | equalityResult -> 
      // in the case where the next token after the evaluated "equality" rule is not an equals
      // sign, then the result of the equality rule evaluation matches the second case of the 
      // assignment rule, and we can return that equality rule result
      return equalityResult
  } 
and private equality =   makeExprRule comparison [BANG_EQUAL; EQUAL_EQUAL]
and private comparison = makeExprRule term       [GREATER; GREATER_EQUAL; LESS; LESS_EQUAL] 
and private term   =     makeExprRule factor     [MINUS;PLUS] 
and private factor =     makeExprRule unary      [SLASH;STAR] 

and private unary tokens =
  match tokens with
  | (tkn::tkns) ->
    match tkn.Type with
    | BANG | MINUS -> 
      unary tkns
      |> Result.map (fun (expr, rest) -> 
        (Unary (tkn, expr), rest))
    | _ -> primary tokens
  | [] -> Error ({Line=0; Msg="Unexpected end of input"}, [])

and private primary tokens = 
  match tokens with
  | (tkn::tkns) ->
    match tkn.Type with    
    | FALSE        -> Ok (Literal (Bool false), tkns)
    | TRUE         -> Ok (Literal (Bool true), tkns)
    | NIL          -> Ok (Literal Nil, tkns)
    | NUMBER n     -> Ok (Literal (Num n), tkns)
    | STRING s     -> Ok (Literal (Str s), tkns)
    | IDENTIFIER _ -> Ok ((Var tkn), tkns)
    | LEFT_PAREN   ->
      expression tkns
      |> Result.bind (fun (expr,rest) -> 
        match rest with 
        | (t::rest') when t.Type = RIGHT_PAREN -> Ok (Grouping expr, rest')
        | (t::rest') -> Error (FloxError.FromToken t "Expect ')' after expression", rest')
        | [] -> Error (FloxError.FromToken tkn "Expect ')' after expression", tkns)
      )
    | _ -> Error ((FloxError.FromToken tkn "Expecting expression"), tkns)

  | [] -> Error ({Line=0; Msg="Unexpected end of input"}, [])

let private printStatement tokens =
  result {
    let! (expr, rest) = expression tokens
    match rest with
    | (t::rest') when t.Type = SEMICOLON -> return (PrintStmt expr, rest')
    | (t::rest') -> return! Error (FloxError.FromToken t "print statement expects ';' after value", rest')
    | _ -> return! Error ({Line=0;Msg="Unexpected end of input"}, [])
  }

let private exprStatement tokens =
  result {
    let! (expr, rest) = expression tokens
    match rest with
    | (t::rest') when t.Type = SEMICOLON -> return (ExprStmt expr, rest')
    | (t::rest') -> return! Error (FloxError.FromToken t "expected ';' after value", rest')
    | _ -> return! Error ({Line=0;Msg="Unexpected end of input"}, [])
  }

let private statement tokens =
  result {
    match tokens with
    | []                            -> return! Error ({Line=0; Msg="Token expected in statement" }, [])
    | (t::rest) when t.Type = PRINT -> return! printStatement rest
    | tkns                          -> return! exprStatement tkns 
  }

let private varDeclaration (tokens : Token list) : Result<Stmt * Token list, (FloxError * Token list)> =
  match tokens with
  | (t::rest) -> 
    match t.Type with 
    | IDENTIFIER _ ->
      result {
        match rest with 
        | (op::rest') when op.Type = EQUAL ->
          let! (initializer,rest'') = expression rest'
          match rest'' with
          | (semi::afterSemi) when semi.Type = SEMICOLON -> return (VarStmt (t, Some initializer), afterSemi)
          | _ -> return! Error (FloxError.FromToken op "expected ';' after value", rest'')
        | _ -> return! Error({Line=0;Msg="Unexpected end of input"}, [])
      }
    | _ -> Error (FloxError.FromToken t "Identifier expected in variable declaration", tokens)
  | [] -> Error ({Line=0;Msg="Assertion Failed: Unexpected end of variable declaration"}, [])

let private declaration (tokens : Token list) : Result<Stmt * Token list, (FloxError * Token list)> =
  result {
    match tokens with
    | [] -> return! Error ({Line=0; Msg="Assertion Failed: Token expected in declaration parser"}, [])
    | (t::rest) when t.Type = VAR -> return! varDeclaration rest
    | _  -> return! statement tokens
  }

let parse tokens =
  let rec loop acc tkns =
    match declaration tkns with
    | Ok (stmt, [])    -> List.rev (stmt::acc) |> Ok
    | Ok (stmt, tkns') -> loop (stmt::acc) tkns'
    | Error (e, _) ->
      // here's where synchronize goes - we'll toss out tokens until we get to a "reset" point - tbd
      Error e
  loop [] tokens