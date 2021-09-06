module Flox.Expressions

open Flox.Tokens
open Flox.Lib
open System.Text
type Value  = Str of string
            | Num of float
            | Bool of bool
            | Nil
with 
  override this.ToString () =
    match this with
    | Str s -> s
    | Num n -> string n 
    | Bool b -> string b 
    | Nil -> "nil"
type Expr = Binary of Expr * Token * Expr
          | Grouping of Expr
          | Literal of Value
          | Unary of Token * Expr
          | Variable of Token

let rec private paren name (exprs: Expr list) =
  let sb = StringBuilder()
  sbAppend "( " sb  
  |> sbAppend name 
  |> ignore
  exprs 
  |> List.iter (fun e -> sbAppend " " sb |> sbAppend (exprToString e) |> ignore)
  sbAppend ")" sb |> ignore
  sb.ToString ()

and exprToString = function
| Literal ob             -> (string ob)
| Unary (token,rhs)      -> paren token.Lexeme [rhs]
| Grouping expr          -> paren "group" [expr]
| Binary (lhs,token,rhs) -> paren token.Lexeme [lhs;rhs]
| Variable token         -> token.Lexeme