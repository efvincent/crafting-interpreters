module Flox.Environment

open Flox.Expressions

type Scope = {
  Id: int
  Vars: Map<string, Value>
  ParentId: int option
} with
  /// Create a new scope with a unique ID
  static member Init id =
    { Vars = Map.empty 
    ; ParentId = None    
    ; Id = id }
  override this.ToString () =
    let pid = Option.defaultValue 0 this.ParentId
    let vars =
      this.Vars
      |> Map.toList
      |> List.sortBy (fun (k,v) -> k)
      |> List.map (fun (k,v) -> sprintf "%s = %A" k v)
      |> String.concat "\n"
      |> fun s -> if s = "" then "No Variables" else s    
    sprintf "%i:%i\n%s" this.Id pid vars

/// Record tracks the root scope of an environment, and a map of scope IDs to scopes.
/// Note: It would be more efficient for Scopes to be a `Map<int, (Scope * int list)>`
/// where the `int list` would be the list of children. The way it currently stands, 
/// to delete a scope requires sequential search of all scopes for each scope descended
/// from a scope being deleted.
type Env = {
  NextScopeId: int
  StatementCount: int
  RootScopeId: int
  Scopes: Map<int, Scope>
} with
  /// Create a new environment (there should only be one per interpreter at this time) with
  /// a new root scope
  static member Init () =
    let root = Scope.Init 1
    { NextScopeId = 2
    ; StatementCount = 0
    ; RootScopeId = root.Id
    ; Scopes = Map.add root.Id root Map.empty }

  override this.ToString () =
    let scopes = 
      this.Scopes
      |> Map.toList
      |> List.sortBy (fun (k,_) -> k)
      |> List.map (snd >> string)
      |> String.concat "\n-------------"
    sprintf "statements: %i\nscopes:\n%s" this.StatementCount scopes

  /// Given a scopeId option, returns the scope from the environment with that ID,
  /// or the root scope if the Id is not found or is None
  member this.GetScope (scopeId:int option) =
    match scopeId with
    | Some sid -> 
      match Map.tryFind sid this.Scopes with
      | Some s -> s
      | None -> Map.find this.RootScopeId this.Scopes
    | None -> Map.find this.RootScopeId this.Scopes

  /// Adds a new scope to the environment with optional parent Id. If the parent Id is none or
  /// doesn't exist, the root scope is used. Note that the parent ID on the new scope will be
  /// correct even if an invalid parent Id is sent to the function (it will be the ID of the
  /// root scope)
  member this.AddScope parentId =
    let parent = this.GetScope parentId
    let newScope = { Scope.Init this.NextScopeId with ParentId = Some parent.Id }
    ({ this with 
        Scopes = Map.add newScope.Id newScope this.Scopes 
        NextScopeId = this.NextScopeId + 1
     }, newScope)

  /// Returns `Some id` of the scope in which the variable is bound, following the line of parent IDs up the
  /// scope hierarchy, or `None` if the variable is not bound in this scope or an anscestor
  member this.ScopeWhereBound vname scopeId =
    let scope = this.GetScope scopeId
    if Map.containsKey vname scope.Vars then Some scope.Id
    elif scope.Id <> this.RootScopeId then
      match scope.ParentId with 
      | Some pid -> this.ScopeWhereBound vname (Some pid)
      | None -> None
    else None

  /// Binds a variable value to the variable name within the scope requested (or root scope if None)
  member this.BindVar vname value scopeId =
    let scope = this.GetScope scopeId
    let scope' = { scope with Vars = Map.add vname value scope.Vars }
    { this with Scopes = (Map.add scope'.Id scope' this.Scopes )}

  /// Gets the value bound to the variable name in the selected scope, if it exists, None otherwise
  member this.GetValue vname scopeId =
    let scope = this.GetScope scopeId
    match (Map.tryFind vname scope.Vars, scope.ParentId) with
    | Some v, _ -> Some v
    | None, None -> None
    | None, pid -> this.GetValue vname pid

  /// Drops the identified scope from the environment, and any scopes with that scope as the parent, so 
  /// long as the requested scope is not the root scope. If the request is made to drop the root scope,
  /// it is ignored. See the note about efficiency in the definition of Env
  member this.DropScope scopeId =
    if scopeId = this.RootScopeId 
    then this 
    else
      let childrenOf id =
        this.Scopes
        |> Map.toList
        |> List.filter (
          fun (_, s) -> 
            match s.ParentId with 
            | None -> false 
            | Some id' -> id = id' && id <> this.RootScopeId)
        |> List.map fst
      let rec go (acc:Set<int>) (ids:int seq) =
        let children = Seq.collect childrenOf ids |> Set.ofSeq    // note: collect === flatMap
        if children.Count = 0 then acc
        else go (Set.union acc children) children
      let rec remove m = function 
        | [] -> m
        | (id::rest) -> remove (Map.remove id m) rest
      let scopes' = 
        go (Set.singleton scopeId) (childrenOf scopeId) 
        |> Set.toList 
        |> remove this.Scopes 
      { this with Scopes = scopes' }
