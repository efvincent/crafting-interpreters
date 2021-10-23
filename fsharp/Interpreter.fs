module Flox.Interpreter

open System
open Flox.Tokens
open Flox.ErrorHandling
open Flox.Expressions
open Flox.Statements
open Flox.Environment

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

let rec eval env scopeId expr : Result<(Env * Value), FloxError> = 
  match expr with 
  | Literal v -> Ok (env, v)
  | Unary (t, e) ->
    result {
      let! value = eval env scopeId e
      match (t.Type, value) with
      | (MINUS, (env', Num n)) -> return (env', Num (0.0 - n))
      | (BANG, (env', Bool b)) -> return (env', Bool (not b))
      | et -> return! Error (FloxError.FromToken t (sprintf "ASSERTION FAIL: Invalid unary token: %A" et))
    }
  | Binary (lhs,t,rhs) ->
    result {
      let! (env', lhsv)  = eval env scopeId lhs
      let! (env'', rhsv) = eval env' scopeId rhs
      let! value =
        match t.Type with 
        | PLUS  -> evalAdd t lhsv rhsv
        | MINUS -> evalNumericBinary t (-) lhsv rhsv
        | STAR  -> evalNumericBinary t (*) lhsv rhsv
        | SLASH -> evalNumericBinary t (/) lhsv rhsv
        | op    -> evalBoolOp t op lhsv rhsv
      return (env'', value)
    }
  | Logical (lhs,op,rhs) -> evalLogicalOp op env scopeId lhs rhs
  | Grouping expr -> eval env scopeId expr
  | Var t ->
    match env.GetValue t.Lexeme scopeId with
    | Some v -> Ok (env, v)
    | None -> Error (FloxError.FromToken t (sprintf "Undefined variable '%s'" t.Lexeme))
  | Assignment (lhs, rhs) ->
    result {
      match env.ScopeWhereBound lhs.Lexeme scopeId with
      | Some sid -> 
        let! (env', rVal) = eval env scopeId rhs
        return (env'.BindVar lhs.Lexeme rVal (Some sid), rVal)
      | None ->
        return! Error <| (FloxError.FromToken lhs (sprintf "Variable '%s' does not exist" lhs.Lexeme))
    }

/// evaluate a logical operation (either AND or OR). Function short circuits logic depending on the operator
and evalLogicalOp op env scopeId lhs rhs =
  let shortCircuit = op = Or
  result {    
    let! (env', lhv) = eval env scopeId lhs
    match toBool () lhv with
    | Ok v when v = shortCircuit ->
      return (env', (Bool shortCircuit))
    | Ok _ ->
      let! (env'', rhv) = eval env' scopeId rhs
      match toBool () rhv with
      | Ok v -> return (env'', (Bool v))
      | Error _ -> return! Error {Line=0;Msg="Right hand side of logical operator not valid boolean"}
    | Error _ -> return! Error {Line=0;Msg="Left hand side of logical operator not valid boolean"}
  }

and blockStmt (environment:Env) scopeId (stmts: Stmt list) : Result<Env, FloxError> =
  result {
    let rec loop env sid statements =
      result {      
        match statements with 
        | stmt::rest -> 
          let! env' = evalStmt env sid stmt
          return! loop env' sid rest
        | [] -> return env
      }
    // TODO: the block scope will be orphaned if we short circuit out of the `result { }` block due to
    // an error
    let (env, blockScope) = environment.AddScope scopeId
    let! env' = loop env (Some blockScope.Id) stmts
    return env'.DropScope blockScope.Id
  }

and ifStmt env scopeId cond ifBlock elseBlockOpt =
  result {
      let! postCondEnv, condVal = eval env scopeId cond
      let! boolVal = toBool None condVal
      let! postIfEnv = 
        if boolVal then 
          evalStmt postCondEnv scopeId ifBlock
        else
          match elseBlockOpt with
          | Some elseBlock -> evalStmt postCondEnv scopeId elseBlock            
          | None -> Ok postCondEnv
      return { postIfEnv with StatementCount = postIfEnv.StatementCount + 1 }    
  }   

and evalStmt (env:Env) (scopeId: int option) (stmt:Stmt) : Result<Env, FloxError> =
  result {
    match stmt with
    | Block stmts ->
      return! blockStmt env scopeId stmts
    | IfStmt (cond, ifBlock, elseBlockOpt) ->
      let! env' = ifStmt env scopeId cond ifBlock elseBlockOpt
      return { env' with StatementCount = env'.StatementCount + 1 }
    | ExprStmt expr ->
      let! (env', _) = eval env scopeId expr
      return { env' with StatementCount = env'.StatementCount + 1 }
    | PrintStmt expr ->
      let! (env',v) = eval env scopeId expr
      printf "%s\n" (string v)
      return { env' with StatementCount = env'.StatementCount + 1 }
    | VarStmt (t, Some e) ->
      let! (env', v) = eval env scopeId e
      let env'' = env'.BindVar t.Lexeme v scopeId 
      return { env'' with StatementCount = env'.StatementCount + 1 }
    | VarStmt (t, None) ->
      return { (env.BindVar t.Lexeme Nil scopeId) with 
                StatementCount = env.StatementCount + 1 } 
  }

let rec interpret istate = function
| (stmt::rest) -> 
  match evalStmt istate None stmt with 
  | Ok (newState) -> interpret newState rest
  | Error e -> Error (e, istate)
| [] -> Ok istate