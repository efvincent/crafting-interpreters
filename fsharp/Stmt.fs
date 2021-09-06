module Flox.Statements

open Flox.Tokens
open Flox.Expressions

type Stmt = 
| ExprStmt of Expr
| PrintStmt of Expr
| Var of Token * Expr
