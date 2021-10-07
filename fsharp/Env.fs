module Flox.Environment

open Flox.Expressions
open Flox.ErrorHandling.ResultCE

type Env = {
  Id: int
  Vars: Map<string, Value>
  Parent: Env option
} with 
  static member Init () = 
    { Vars   = Map.empty 
    ; Parent = None
    ; Id     = let g = System.Guid.NewGuid() in g.GetHashCode()
    }

  /// insert or update variable binding in this environment
  member this.UpsertVariable n v =
    { this with Vars = Map.add n v this.Vars }

  /// returns true if the variable exists either in this environment or a parent environment
  member this.VarExists n =
    match Map.containsKey n this.Vars, this.Parent with
    | (false, Some parent) -> parent.VarExists n
    | (false, None) -> false
    | (true, _) -> true

  /// Retrieves a value with a name `n` from the environment, or delegates
  /// to the parent environment if it exists
  member this.GetValue n =
    match Map.tryFind n this.Vars, this.Parent with
    | None, Some parent -> parent.GetValue n
    | None, None        -> None
    | Some v, _         -> Some v

type InterpreterState = {
  StatementCount : int
  RootEnv : Env
  Environments : Map<int, Env>
} with
  static member Init () =
    let rootEnv = Env.Init() in 
    { StatementCount = 0
    ; RootEnv = rootEnv
    ; Environments = Map.empty.Add(rootEnv.Id, rootEnv) }
  
  /// insert or update a variable binding in the appropriate environment and
  /// return updated interpreter state or an error
  member this.UpsertVariable envId n v =
    this.GetEnv (Some envId)
    |> Result.map (fun (env:Env) -> 
        { this with Environments = Map.add envId (env.UpsertVariable n v) this.Environments } 
      )

  /// Gets the requested environment or returns an error     
  member this.GetEnv = function
    | None -> Ok this.RootEnv
    | Some envId -> 
      match Map.tryFind envId this.Environments with
      | Some env -> Ok env
      | None -> Error {Tokens.FloxError.Line=0; Tokens.FloxError.Msg=(sprintf "Uknown environment with id: %i" envId )}

  /// Creates a new environment with the parent ID set to environment with the passed ID, 
  /// or returns an error if there is no such environment
  member this.NewEnvironment parentEnvId =
    this.GetEnv (Some parentEnvId)
    |> Result.map (fun parent ->
        let env = { Env.Init () with Parent = Some parent }
        { this with Environments = Map.add env.Id env this.Environments }
      )

  /// Remove an environment from the interpreter state, this happens when a scope is closed. Note that
  /// this does not handle the case of closures where it's possible for a variable to exist both in its
  /// scope and in a new environment created by the closure. Closures will be interesting to solve.
  member this.CloseEnvironment envId =
    Ok {this with Environments = Map.remove envId this.Environments }
