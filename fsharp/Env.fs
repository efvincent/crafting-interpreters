module Flox.Environment

open Flox.Expressions

type Env = Map<string, Value>

let defineVar (e:Env) n v : Env =
  Map.add n v e
  
let getValue (e:Env) n : Value option =
  Map.tryFind n e