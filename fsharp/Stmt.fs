module Flox.Statements

open Flox.Tokens
open Flox.Expressions

type Stmt = 
| Block of Stmt list
| ExprStmt of Expr
| PrintStmt of Expr
| IfStmt of Expr * Stmt * Stmt option
| WhileStmt of Expr * Stmt
| VarStmt of Token * Expr option
