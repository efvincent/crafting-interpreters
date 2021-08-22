open System
open System.IO
open Flox.Tokens
open Flox.FScanner

let runPrompt () =
  Ok ()

let run source =
  scan source

let runFile fn =
  printfn "Processing file %s" fn
  let rec go (tkns:Token list) =
    match tkns with
    | (t::ts) -> 
      printfn "Line %i %A" t.Line t.Type
      go ts
    | [] -> Ok ()
  if File.Exists fn then
    match run <| File.ReadAllText fn with
    | Ok tokens -> go tokens
    | Error e -> Error e
  else
    Error { Line = 0; Msg = "File Not found" }
 
[<EntryPoint>]
let main argv =
  let result = 
    if argv.Length < 1 then
      runPrompt ()
    elif argv.Length = 1 then
      runFile argv.[0]
    else
      printfn("Usage: flox [script]\n")
      Ok () 
  match result with 
  | Ok () -> printfn "\nDone.\n"
  | Error e -> printfn "Error line %i - %s" e.Line e.Msg
  0

