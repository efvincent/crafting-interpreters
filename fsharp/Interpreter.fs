module Flox.Interpreter

open System
open Flox.Expressions
open Flox.Tokens
open Flox.ErrorHandling

let toNum = function
  | Num d -> Ok d
  | Str s -> 
    match Double.TryParse s with
    | (true, n)  -> Ok n
    | (false, _) ->      
      Error {Line=0; Msg=(sprintf """Type Mismatch. String "%s" cannot be interpreted as a number""" s) } 
  | v -> Error {Line=0; Msg=(sprintf """Type Mismatch. Value "%A" cannot be interpreted as a number""" v) }

let toBool = function
  | Bool b -> Ok b
  | Num n when n = 0.0 -> Ok false
  | Nil -> Ok false 
  | _ -> Ok true

let toStr = function
  | Bool b -> Ok (string b)
  | Num n  -> Ok (string n)
  | Str s  -> Ok s
  | Nil    -> Ok ""

let rec evalAdd l r =
  result {
      match (l,r) with
      | (Str ls,_) -> 
        let! rs = toStr r
        return (Str (ls + rs)) 
      | (Num ln, _) ->
        let! rn = toNum r
        return (Num (ln + rn))
      | (Nil, _)
      | (_, Nil) -> return Nil
      | (Bool _, _)
      | (_, Bool _) -> return! Error { Line=0; Msg="Type Mismatch. Addition requires numbers or strings" }
  }

let rec evalNumericBinary op l r =
  result {
    let! ln = toNum l
    let! rn = toNum r
    return (Num (op ln rn))
  }

let evalBoolOp op l r =
  result {
    let! bfn = 
      match op with
      | EQUAL_EQUAL   -> Ok (=)
      | BANG_EQUAL    -> Ok (<>)
      | GREATER       -> Ok (>)
      | GREATER_EQUAL -> Ok (>=)
      | LESS          -> Ok (<)
      | LESS_EQUAL    -> Ok (<=)
      | invalidOp     -> Error { Line=0; Msg=(sprintf "Invalid Token %A in expression" invalidOp) }

    let! bl = toBool l
    let! br = toBool r
    return Bool (bfn bl br)
  }

let rec eval = function
| Literal v -> Ok v
| Unary (t, e) ->
  eval e
  |> Result.bind (fun value -> 
    match (t.Type, value) with
    | (MINUS, (Num n)) -> Ok (Num (0.0 - n))  
    | (BANG, (Bool b)) -> Ok (Bool (not b))
    | et -> Error { Line=0; Msg=sprintf "ASSERTION FAIL: Invalid unary token: %A" et }
  )
| Binary (lhs,t,rhs) ->
  result {
    let! lhsv = eval lhs
    let! rhsv = eval rhs
    match t.Type with 
    | PLUS  -> return! evalAdd lhsv rhsv
    | MINUS -> return! evalNumericBinary (-) lhsv rhsv
    | STAR  -> return! evalNumericBinary (*) lhsv rhsv
    | SLASH -> return! evalNumericBinary (/) lhsv rhsv
    | op    -> return! evalBoolOp op lhsv rhsv
  }
| Grouping expr -> eval expr


