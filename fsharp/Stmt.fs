module Flox.Statements

open Flox.Tokens
open Flox.Expressions

type Stmt = 
| ExprStmt of Expr
| PrintStmt of Expr
| VarStmt of Token * Expr option
