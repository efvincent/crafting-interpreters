open System
open System.IO
open Flox.Tokens
open Flox.FScanner
open Flox.Parser
open Flox.Expressions
open Flox.Interpreter

let printSplash () = 
  printfn "FLOX Programming Language\nCopyright 2021 Eric F. Vincent\nVersion 0.21\n\n type #exit or <CTRL-D> to exit\n"

let run source =
  scan source
  |> Result.bind parse

let runPrompt () =
  printSplash () 
  let rec loop env = 
    printf "\x1b[1m\x1b[35mɸλοχ \x1b[33m>\x1b[0m "
    match let input = Console.ReadLine() in if isNull input then "#exit" else input with
    | s when s.ToLower () = "#exit" -> ()
    | s when s.ToLower () = "#env" -> 
      printfn "%s" (string env)
      loop env
    | input ->      
      match run input with
      | Ok stmts -> 
        printfn "\x1b[36m"
        match interpret env stmts with
        | Ok istate' -> 
          printf "\x1b[0m" 
          loop istate'
        | Error (e, istate') -> 
          printf "\x1b[31m%s\x1b[0m\n" e.Msg
          loop istate'
      | Error e -> 
        printfn "\x1b[31m%s\x1b[0m\n" e.Msg
        loop env
  Ok <| loop (Flox.Environment.Env.Init ())

let runFile fn =
  printfn "Processing file %s" fn
  let rec go (tkns:Token list) =
    match tkns with
    | (t::ts) -> 
      printfn "Line %i %A" t.Line t.Type
      go ts
    | [] -> Ok ()
  if File.Exists fn then
    match scan <| File.ReadAllText fn with
    | Ok tokens -> go tokens
    | Error e -> Error e
  else
    Error { Line = 0; Msg = "File Not found" }
 
[<EntryPoint>]
let main argv =
  (if argv.Length < 1 then
    runPrompt ()
  elif argv.Length = 1 then
    runFile argv.[0]
  else
    printfn("Usage: flox [script]\n")
    Ok ())  
  |> Result.map (fun _ -> printfn "\nDone.\n")
  |> Result.mapError (fun e -> printfn "Error line %i - %s" e.Line e.Msg)
  |> ignore
  0

