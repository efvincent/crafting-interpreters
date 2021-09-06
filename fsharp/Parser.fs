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

expression  -> equality ;

equality    -> comparison ( ( "!=" | "==") comparison )* ;

comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term        -> factor ( ( "-" | "*" ) factor )* ;

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

let rec private expression = equality
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
    | FALSE -> (Ok (Literal (Bool false), tkns))
    | TRUE  -> (Ok (Literal (Bool true), tkns))
    | NIL   -> (Ok (Literal Nil, tkns))
    | NUMBER n -> (Ok (Literal (Num n), tkns))
    | STRING s -> (Ok (Literal (Str s), tkns))
    | LEFT_PAREN ->
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
  let rec loop acc tkns =
    result {
      match tkns with 
      | [] -> return acc
      | (t::rest) when t.Type = PRINT -> 
        let! (stmt, rest') = printStatement rest 
        return! loop (stmt::acc) rest'    
      | tkns -> 
        let! (stmt, rest') = exprStatement tkns
        return! loop (stmt::acc) rest'
    }
  loop [] tokens

let parse tokens =
  statement tokens 
  |> Result.mapError fst 
