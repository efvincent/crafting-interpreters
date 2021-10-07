module Flox.Statements

open Flox.Tokens
open Flox.Expressions

type Stmt = 
| Block of Stmt list
| ExprStmt of Expr
| PrintStmt of Expr
| VarStmt of Token * Expr option
