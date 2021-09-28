module Flox.Environment

open Flox.Expressions

type Env = {
  Vars: Map<string, Value>
  Parent: Env option
} with 
  static member init = { Vars = Map.empty; Parent = None }

/// Given an environment, a name, and a value, stores (or replaces)
/// the value with the name in the environment
let upsertVariable (e:Env) n v : Env =
  { e with Vars = Map.add n v e.Vars }
  
/// Retrieves a value with a name `n` from the environment, or delegates
/// to the parent environment if it exists
let rec getValue (e:Env) n : Value option =
  match Map.tryFind n e.Vars, e.Parent with
  | (None, Some parent) -> getValue parent n
  | (None, None) -> None
  | (Some v, _) -> Some v

let rec exists (e:Env) n =
  match Map.containsKey n e.Vars, e.Parent with 
  | (false, Some parent) -> exists parent n
  | (false, None) -> false
  | (true, _) -> true
  
  