module Flox.Interpreter

open System
open Flox.Tokens
open Flox.ErrorHandling
open Flox.Expressions
open Flox.Statements
open Flox.Environment

type InterpreterState = {
  StatementCount : int
  Environment : Env
} with
  static member init = {
    StatementCount = 0
    Environment = Map.ofList []
  }

let private toNum t = function
  | Num d -> Ok d
  | Str s -> 
    match Double.TryParse s with
    | (true, n)  -> Ok n
    | (false, _) ->      
      Error (FloxError.FromToken t (sprintf """Type Mismatch. String "%s" cannot be interpreted as a number""" s))
  | v -> Error (FloxError.FromToken t (sprintf """Type Mismatch. Value "%A" cannot be interpreted as a number""" v))

let private toBool _ = function
  | Bool b -> Ok b
  | Num n when n = 0.0 -> Ok false
  | Nil -> Ok false 
  | _ -> Ok true

let private toStr _ = function
  | Bool b -> Ok (string b)
  | Num n  -> Ok (string n)
  | Str s  -> Ok s
  | Nil    -> Ok ""

let rec private evalAdd t l r =
  result {
      match (l,r) with
      | (Str ls,_) -> 
        let! rs = toStr t r
        return (Str (ls + rs)) 
      | (Num ln, _) ->
        let! rn = toNum t r
        return (Num (ln + rn))
      | (Nil, _)
      | (_, Nil) -> return Nil
      | (Bool _, _)
      | (_, Bool _) -> return! Error (FloxError.FromToken t "Type Mismatch. Addition requires numbers or strings" )
  }

let rec private evalNumericBinary t op l r =
  result {
    let! ln = toNum t l
    let! rn = toNum t r
    if t.Type = SLASH && rn = 0.0 
    then return! Error (FloxError.FromToken t "Division by zero")
    else return (Num (op ln rn))
  }

let private evalBoolOp t op l r =
  result {
    let! (bfn : IComparable -> IComparable -> bool)= 
      match op with
      | EQUAL_EQUAL   -> Ok (=)
      | BANG_EQUAL    -> Ok (<>)
      | GREATER       -> Ok (>)
      | GREATER_EQUAL -> Ok (>=)
      | LESS          -> Ok (<)
      | LESS_EQUAL    -> Ok (<=)
      | invalidOp     -> Error (FloxError.FromToken t (sprintf "Invalid Token %A in expression" invalidOp))

    match l with
    | Num nl -> 
      let! nr = toNum t r
      return Bool (bfn nl nr)
    | Bool bl ->      
      let! br = toBool t r
      return Bool (bfn bl br)
    | Str sl -> 
      let! sr = toStr t r
      return Bool (bfn sl sr)
    | Nil -> return Bool false
  }

let rec eval istate expr : Result<(InterpreterState * Value), FloxError> = 
  match expr with 
  | Literal v -> Ok (istate, v)
  | Unary (t, e) ->
    result {
      let! value = eval istate e
      match (t.Type, value) with
      | (MINUS, (istate', Num n)) -> return (istate', Num (0.0 - n))
      | (BANG, (istate', Bool b)) -> return (istate', Bool (not b))
      | et -> return! Error (FloxError.FromToken t (sprintf "ASSERTION FAIL: Invalid unary token: %A" et))
    }
  | Binary (lhs,t,rhs) ->
    result {
      let! (istate', lhsv)  = eval istate lhs
      let! (istate'', rhsv) = eval istate' rhs
      let! value =
        match t.Type with 
        | PLUS  -> evalAdd t lhsv rhsv
        | MINUS -> evalNumericBinary t (-) lhsv rhsv
        | STAR  -> evalNumericBinary t (*) lhsv rhsv
        | SLASH -> evalNumericBinary t (/) lhsv rhsv
        | op    -> evalBoolOp t op lhsv rhsv
      return (istate'', value)
    }
  | Grouping expr -> eval istate expr
  | Var t ->
    match getValue istate.Environment t.Lexeme with
    | Some v -> Ok (istate, v)
    | None -> Error (FloxError.FromToken t (sprintf "Undefined variable '%s'" t.Lexeme))
  | Assignment (lhs, rhs) ->
    result {
      let! (istate', rVal) = eval istate rhs
      let istate'' = { istate' with
                        Environment = upsertVariable istate'.Environment lhs.Lexeme rVal }
      return (istate'', Nil)
    }

let evalStmt (istate:InterpreterState) (stmt:Stmt) : Result<InterpreterState, FloxError> =
  result {
    match stmt with
    | ExprStmt expr ->
      let! (istate', _) = eval istate expr
      return { istate' with StatementCount = istate.StatementCount + 1 }
    | PrintStmt expr ->
      let! (_,v) = eval istate expr
      printf "%s\n" (string v)
      return { istate with StatementCount = istate.StatementCount + 1 }
    | VarStmt (t, Some e) ->
      let! (istate', v) = eval istate e
      return { istate' with 
                StatementCount = istate.StatementCount + 1
                Environment = upsertVariable istate.Environment t.Lexeme v }
    | VarStmt (t, None) ->
      return { istate with
                StatementCount = istate.StatementCount + 1
                Environment = upsertVariable istate.Environment t.Lexeme Nil }
  }

let rec interpret istate = function
| (stmt::rest) -> 
  match evalStmt istate stmt with 
  | Ok (newState) -> interpret newState rest
  | Error e -> Error (e, istate)
| [] -> Ok istate