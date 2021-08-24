module Flox.Tokens

type TokenType =
      // Single-character tokens
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE 
  | COMMA 
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  
  // one or two character tokens
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL

  // literals
  | IDENTIFIER of string
  | STRING of string
  | NUMBER of float

  // keywords
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF

type Token = 
  { Type: TokenType
    Lexeme : string
    Line: int }

type FloxError = 
  { Line: int
    Msg: string }  
  static member FromToken (t:Token) msg = 
      let s = (sprintf "[line %i] Error %s" t.Line msg)
      { Line = t.Line; Msg = s }