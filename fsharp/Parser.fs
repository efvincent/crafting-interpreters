module Flox.Parser

// Suppress a warning relating to the initialization soundness of recursively defined bindings
// that are created using the `makeRule` function below
#nowarn "40"  

open Flox.Tokens
open Flox.Expressions

let private makeRule nextRule matchTypes =
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
and private equality =   makeRule comparison [BANG_EQUAL; EQUAL_EQUAL]
and private comparison = makeRule term       [GREATER; GREATER_EQUAL; LESS; LESS_EQUAL] 
and private term   =     makeRule factor     [MINUS;PLUS] 
and private factor =     makeRule unary      [SLASH;STAR] 

and private unary tokens =
  match tokens with
  | (tkn::tkns) ->
    match tkn.Type with
    | BANG | MINUS -> 
      unary tkns
      |> Result.map (fun (expr, rest) -> 
        (Unary (tkn, expr), rest))
    | _ -> primary tokens
  | [] -> Error ({Line=0; Msg="No tokens at unary"}, [])

and private primary tokens = 
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
        | [] -> Error (FloxError.FromToken tkn "Expect ')' after expression", tkns)
      )
    | _ -> Error ((FloxError.FromToken tkn "Expecting expression"), tkns)

  | [] -> Error ({Line=0; Msg="No tokens at primary"}, [])

let parse tokens =
  expression tokens 
  |> Result.mapError fst 
  |> Result.map fst