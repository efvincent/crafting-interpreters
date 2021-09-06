module Flox.Statements

open Flox.Tokens
open Flox.Lib
open Flox.Expressions
open System.Text 

type Stmt = 
| ExprStmt of Expr
| PrintStmt of Expr
