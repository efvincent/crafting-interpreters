module Flox.Parser

open Flox.Tokens
open Flox.Expressions

let rec expression (tokens : Token list) : Result<Expr * Token list,FloxError * Token list> =
  raise (System.NotImplementedException())

and equality tokens =
  comparison tokens
  |> Result.bind (fun (expr, rest) ->
    match rest with
    | (t::rest') when List.contains t.Type [BANG_EQUAL;EQUAL_EQUAL] ->
      comparison rest'
      |> Result.map (fun (right, rest'') -> Binary (expr, t, right), rest'')
    | _ -> Ok (expr, rest)
  )

and comparison tokens =
  term tokens
  |> Result.bind (fun (expr, rest) ->
    match rest with
    | (t::rest') when List.contains t.Type [GREATER;LESS;GREATER_EQUAL;LESS_EQUAL] ->
      term rest'
      |> Result.map (fun (right, rest'') -> Binary (expr, t, right), rest'')
    | _ -> Ok (expr, rest)
  )

and term tokens =
  factor tokens
  |> Result.bind (fun (expr, rest) ->
    match rest with
    | (t::rest') when List.contains t.Type [MINUS;PLUS] ->
      factor rest'
      |> Result.map (fun (right, rest'') -> Binary (expr, t, right), rest'')
    | _ -> Ok (expr, rest)
  )

and factor tokens = 
  unary tokens
  |> Result.bind (fun (expr,rest) ->
    match rest with
    | (t::rest') when List.contains t.Type [SLASH;STAR] ->
      unary rest'
      |> Result.map (fun (right, rest'') -> Binary (expr, t, right), rest'')
    | _ -> Ok (expr, rest)
  )

and unary tokens : Result<Expr * Token list, FloxError * Token list>=
  match tokens with
  | (tkn::tkns) ->
    match tkn.Type with
    | BANG | MINUS -> 
      unary tkns
      |> Result.map (fun (expr, rest) -> 
        (Unary (tkn, expr), rest))
    | _ -> primary tokens
  | [] -> Error ({Line=0; Msg="No tokens at unary"}, [])

and primary tokens : Result<Expr * Token list, FloxError * Token list> =
  match tokens with
  | (tkn::tkns) ->
    match tkn.Type with    
    | FALSE -> (Ok (Literal false, tkns))
    | TRUE  -> (Ok (Literal true, tkns))
    | NIL   -> (Ok (Literal null, tkns))
    | NUMBER n -> (Ok (Literal n, tkns))
    | STRING s -> (Ok (Literal s, tkns))
    | LEFT_PAREN ->
      expression tkns
      |> Result.bind (fun (expr,rest) -> 
        match rest with 
        | (t::rest') when t.Type = RIGHT_PAREN -> Ok (Grouping expr, rest')
        | (t::rest') -> Error (FloxError.FromToken t "Expect ')' after expression", rest')
        | [] -> Error ({Line=0; Msg="ASSERTION-FAILED: Unexpected end of file" }, [])
      )
    | _ -> Error ((FloxError.FromToken tkn "Expecting expression"), tkns)

  | [] -> Error ({Line=0; Msg="No tokens at primary"}, [])