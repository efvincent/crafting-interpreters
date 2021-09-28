module Flox.Environment

open Flox.Expressions

type Env = Map<string, Value>

/// Given an environment, a name, and a value, stores (or replaces)
/// the value with the name in the environment
let upsertVariable (e:Env) n v : Env =
  Map.add n v e
  
/// Retrieves a value with a name `n` from the environment
let getValue (e:Env) n : Value option =
  Map.tryFind n e