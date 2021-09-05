module Flox.Expressions

open Flox.Tokens

type Value  = Str of string
            | Num of float
            | Bool of bool
            | Nil
type Expr = Binary of Expr * Token * Expr
          | Grouping of Expr
          | Literal of Value
          | Unary of Token * Expr