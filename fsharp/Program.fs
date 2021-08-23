open System
open System.IO
open System.Text
open Flox.Tokens
open Flox.FScanner
open Flox.Parser
open Flox.Expressions
open Flox.Lib

let run source =
  scan source
  |> Result.bind parse

let rec paren name (exprs: Expr list) =
  let sb = StringBuilder()
  sbAppend "( " sb  
  |> sbAppend name 
  |> ignore
  exprs 
  |> List.iter (fun e -> sbAppend " " sb |> sbAppend (exprToString e) |> ignore)
  sbAppend ")" sb |> ignore
  sb.ToString ()

and exprToString = function
| Literal ob -> if isNull ob then "nil" else (string ob)
| Unary (token,rhs) -> paren token.Lexeme [rhs]
| Grouping expr -> paren "group" [expr]
| Binary (lhs,token,rhs) -> paren token.Lexeme [lhs;rhs]

let runPrompt () =
  let rec loop () = 
    printf "> "
    let input = Console.ReadLine()
    if not (isNull input) then
      match run input with
      | Ok expr -> printfn "%s" (exprToString expr); loop ()
      | Error e -> printfn "%s" e.Msg; loop ()
    else
      printfn "\nGoodbye.\n"
  Ok <| loop ()

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

