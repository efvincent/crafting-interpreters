module Flox.Expressions

open Flox.Tokens

type Expr = Binary of Expr * Token * Expr
          | Grouping of Expr
          | Literal of obj
          | Unary of Token * Expr