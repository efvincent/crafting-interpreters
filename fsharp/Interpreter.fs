module Flox.Interpreter

open Flox.Expressions
open Flox.Tokens
open Flox.ErrorHandling

let toNum (ob:obj) =
  match ob with 
  | :? float as d -> Ok d
  | _ ->
    let n = if isNull ob then "null" else ob.GetType().Name 
    Error {Line=0; Msg=(sprintf "Type Mismatch. Value '%A' has type %s, Number expected" ob n) } 

let toBool (ob:obj) =
  match ob with
  | :? bool as b -> Ok b
  | :? float as d when d = 0.0 -> Ok false
  | null -> Ok false
  | _ -> Ok true

let toString (ob:obj) =
  Ok <| string ob

let evalAdd l r : float =
  if l :? string or r :? string then Ok ((string l) + (string r)) 
  elif l :? float and f :? float then Ok ()

let rec eval = function
| Literal ob -> Ok ob
| Unary (t, e) ->
  eval e
  |> Result.bind (fun value -> 
    match t.Type with
    | MINUS when (value :? float) -> Ok <| box (0.0 - (value :?> float))
    | BANG  when (value :? bool)   -> Ok <| box (not (value :?> bool))
    | MINUS -> Error { Line=0; Msg="Type Mismatch. Number expected for unary minus" }
    | BANG  -> Error { Line=0; Msg="Type Mismatch. Boolean expected for unary not (!)" }
    | et -> Error { Line=0; Msg=sprintf "ASSERTION FAIL: Invalid unary token: %A" et }
  )
| Binary (lhs,t,rhs) ->
  result {
    let! lhsv = eval lhs
    let! rhsv = eval rhs
    match t.Type with 
    | PLUS -> evalAdd lhsv rhsv
    | tt -> return! (Error {Line=0;Msg=sprintf "%A not yet implemented" tt })
  }
| Grouping expr -> eval expr


