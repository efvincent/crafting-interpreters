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
              | ifStmt
              | printStmt 
              | whileStmt
              | block ;

whileStmt   -> "while" "(" expression ")" statement ;

ifStmt      -> "if" "(" expression ")" statement ("else" statement )? ;

block       -> "{" declaration* "}" ;

exprStmt    -> expression ";" ;

printStmt   -> "print" expression ";" ;

expression  -> assignment ;

assignment  -> IDENTIFIER "=" assignment | logic_or;

logic_or    -> logic_and ( "or" logic_and )*;

logic_and   -> equality ( "and" equality )*;

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

/// Makes binary expression rules. It will use the `nextRule` rule to evaluate either
/// side of the binary operator. If the left side succeeds and the operator matches one of
/// the list of provided tokens, evaluates the right side to complete the binary expression
let private makeBinaryExprRule nextRule matchTypes =
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

let private makeLogicRule ttype next tkns =
  let logOp = if ttype = OR then Or else And
  result {
    match! next tkns with
    | (lhs, tkn::rest) when tkn.Type = ttype ->
      let! (rhs,tokens) = next rest
      return (Logical (lhs, logOp, rhs), tokens)
    | andResult -> return andResult
  }

/// The top level expression rule, all expression variants are cases of this rule. It is
/// an alias of the assignment rule
let rec private expression = assignment

/// assignment  -> IDENTIFIER "=" assignment | equality;
/// If we have an identifier token followed by the equals token, we evaluate the right side
/// with the assignment rule, otherwise we pass the tokens to the equality rule
and private assignment tokens =
  result {
    // evaluate an equality, which would consume up to, but not including the equals
    match! logicOr tokens with
    | (Var name, equals::rest) when equals.Type = EQUAL -> 
      // in the case where we have a variable and an equals, the "rest" should have an expression
      // for the right hand side of the assignment. Get that rhs, build and return the assignment
      let! (rhs, tkns') = assignment rest
      return (Assignment (name, rhs), tkns')
    | (_, equals::rest) when equals.Type = EQUAL ->
      // in the case where we have a something (not a variable) and an equals sign, that's an
      // invalid assignment target
      return! Error <| (FloxError.FromToken (List.head tokens) "Invalid Assignment Target", rest)
    | logicResult -> 
      // in the case where the next token after the evaluated "logicOr" rule is not an equals
      // sign, then the result of the logical Or rule evaluation matches the second case of the 
      // assignment rule, and we can return that logical or rule result
      return logicResult
  } 

and private logicOr = makeLogicRule OR logicAnd

and private logicAnd = makeLogicRule AND equality

/// binary expression matching != or == as the binary operator between two expressions parsed
/// by the `comparison` rule
and private equality =   makeBinaryExprRule comparison [BANG_EQUAL; EQUAL_EQUAL]

/// binary expression matching one of (> >= < <=) as the binary operator between
/// two expressions parsed by the `term` rule
and private comparison = makeBinaryExprRule term       [GREATER; GREATER_EQUAL; LESS; LESS_EQUAL]

/// binary expression rule matching one of (-, +) as the binary operator between
/// two expressions parsed by the `factor` rule
and private term   =     makeBinaryExprRule factor     [MINUS;PLUS] 

/// binary expression rule matching one of (/, *) as the binary operator between
/// two expressions parsed by the `unary` rule
and private factor =     makeBinaryExprRule unary      [SLASH;STAR] 

/// rule to parse unary expressions, such as not (!) and negation (-)
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
        | t::rest' when t.Type = RIGHT_PAREN -> Ok (Grouping expr, rest')
        | t::rest' -> Error (FloxError.FromToken t "Expect ')' after expression", rest')
        | [] -> Error (FloxError.FromToken tkn "Expect ')' after expression", tkns)
      )
    | _ -> Error ((FloxError.FromToken tkn "Expecting expression"), tkns)

  | [] -> Error ({Line=0; Msg="Unexpected end of input"}, [])

let private printStatement tokens =
  result {
    let! expr, rest = expression tokens
    match rest with
    | t::rest' when t.Type = SEMICOLON -> return (PrintStmt expr, rest')
    | t::rest' -> return! Error (FloxError.FromToken t "print statement expects ';' after value", rest')
    | _ -> return! Error ({Line=0;Msg="Unexpected end of input"}, [])
  }

let private exprStatement tokens =
  result {
    let! expr, rest = expression tokens
    match rest with
    | t::rest' when t.Type = SEMICOLON -> return (ExprStmt expr, rest')
    | t::rest' -> return! Error (FloxError.FromToken t "expected ';' after value", rest')
    | _ -> return! Error ({Line=0;Msg="Unexpected end of input"}, [])
  }

let rec private ifStatement tokens = 
  result {
    match tokens with
    | t::rest when t.Type = LEFT_PAREN ->
      let! cond, postCond = expression rest
      match postCond with
      | t::postRParen when t.Type = RIGHT_PAREN ->
        let! thenStmt, postThen = statement postRParen
        let! elseStmtOpt = 
          result {
            match postThen with
            | t::postElse when t.Type = ELSE ->
              let! (elseStmt, tkns) = statement postElse
              return Some (elseStmt, tkns)
            | _ -> return None
          }
        match elseStmtOpt with
        | Some (elseStmt, postElseBlock) ->
          return (IfStmt(cond, thenStmt, Some elseStmt), postElseBlock)
        | None ->
          return (IfStmt(cond, thenStmt, None), postThen)
      | t::tkns -> return! Error (FloxError.FromToken t "expected closing paren in if condition", tkns)
      | _ -> return! Error ({Line=0;Msg="Unexpected end of input"}, [])
    | t::tkns -> return! Error (FloxError.FromToken t "expected left paren in if condition", tkns)
    | _ -> return! Error ({Line=0;Msg="Unexpected end of input"}, [])
  }

and private whileStatement tokens : Result<(Stmt * Token list), (FloxError * Token list)> =
  result {
    match tokens with
    | t::rest when t.Type = LEFT_PAREN -> 
      let! cond, postCond = expression rest
      match postCond with
      | t::postRParen when t.Type = RIGHT_PAREN ->
        let! stmt, postStmt = statement postRParen
        return (WhileStmt(cond, stmt), postStmt)
      | t::tkns -> return! Error (FloxError.FromToken t "expected closing paren in while condition", tkns)
      | _ -> return! Error ({Line=0;Msg="Unexpected end of input"}, [])
    | t::tkns -> return! Error (FloxError.FromToken t "expected left paren in while condition", tkns)
    | _ -> return! Error ({Line=0;Msg="Unexpected end of input"}, [])
  }

and private declaration tokens =
  result {
    match tokens with
    | [] -> return! Error ({Line=0; Msg="Assertion Failed: Token expected in declaration parser"}, [])
    | t::rest when t.Type = VAR -> return! varDeclaration rest
    | _  -> return! statement tokens
  }

and private statement tokens =
  result {
    match tokens with
    | []                                -> return! Error ({Line=0; Msg="Token expected in statement" }, [])
    | t::rest when t.Type = IF          -> return! ifStatement rest
    | t::rest when t.Type = LEFT_BRACE  -> return! blockStatement rest
    | t::rest when t.Type = PRINT       -> return! printStatement rest
    | t::rest when t.Type = WHILE       -> return! whileStatement rest
    | tkns                              -> return! exprStatement tkns 
  }

and private varDeclaration tokens =
  match tokens with
  | t::rest -> 
    match t.Type with 
    | IDENTIFIER _ ->
      result {
        match rest with 
        | op::rest' when op.Type = SEMICOLON ->
          return (VarStmt (t, None), rest')
        | op::rest' when op.Type = EQUAL ->
          let! initializer, rest'' = expression rest'
          match rest'' with
          | semi::afterSemi when semi.Type = SEMICOLON -> return (VarStmt (t, Some initializer), afterSemi)
          | _ -> return! Error (FloxError.FromToken op "expected ';' after value", rest'')
        | _ -> return! Error({Line=0;Msg="Unexpected end of input"}, [])
      }
    | _ -> Error (FloxError.FromToken t "Identifier expected in variable declaration", tokens)
  | [] -> Error ({Line=0;Msg="Assertion Failed: Unexpected end of variable declaration"}, [])

and private blockStatement tokens =
  result {
    // Upon entry, the opening brace will already have been consumed by the `statement` parser. The first
    // token in the passed list will be the first token inside a block, or the closing brace for an
    // empty block
    let rec loop acc tkns : Result<(Stmt * Token list), (FloxError * Token list)> =
      result {
        match tkns with
        | t::rest when t.Type = RIGHT_BRACE ->
          // we've seen the closing brace, this block is over. We consume the brace
          // and return the reversed list of statements - reversed because we were prepending
          return (Block (List.rev acc), rest)
        | _::_ ->
          // we see a token that's not the last token, and it's not the closing brace, so
          // it belongs to a statement (or expression) inside the block 
          let! (stmt, rest') = declaration tkns
          return! loop (stmt::acc) rest'
        | [] ->
          return! Error ({Line=0;Msg="Unexpected end of input processing statement block" }, [])
      }      
    return! loop [] tokens
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