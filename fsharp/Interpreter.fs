module Flox.Interpreter

open Flox.Expressions
open Flox.Tokens
open Flox.ErrorHandling

let toNum (ob:obj) =
  match ob with 
  | :? double as d -> Ok d
  | _ -> Error {Line=0; Msg="Type Mismatch. Number expected" } 

let rec eval = function
| Literal ob -> Ok ob
| Unary (t, e) ->
  eval e
  |> Result.bind (fun value -> 
    match t.Type with
    | MINUS when (value :? double) -> Ok <| box (0.0 - (value :?> double))
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
    | PLUS ->
      let! x = toNum lhsv
      let! y = toNum rhsv
      return! (Ok (box (x + y)))
    | tt -> return! (Error {Line=0;Msg=sprintf "%A not yet implemented" tt })
  }
 
