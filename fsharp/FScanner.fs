module Flox.FScanner

open Flox.Tokens
open Flox.Lib

let private keywords = 
  [
    ("and", AND);
    ("class", CLASS);
    ("else", ELSE);
    ("false", FALSE);
    ("for", FOR);
    ("fun", FUN);
    ("if", IF);
    ("nil", NIL);
    ("or", OR);
    ("print", PRINT);
    ("return", RETURN);
    ("super", SUPER);
    ("this", THIS);
    ("true", TRUE);
    ("var", VAR);
    ("while", WHILE)
  ]
  |> Map.ofList

type ScanState = {
  Tokens: Token list
  Line: int
}

let rec private skipRemainingLine = function
| ('\n'::cs) -> cs
| [] -> []
| (_::cs) -> skipRemainingLine cs

let private addToken lexeme tokenType ss =
  {ss with Tokens = {Type = tokenType; Lexeme = lexeme; Line = ss.Line} :: ss.Tokens }

let private addTokenC lexeme = addToken (string lexeme)

let private scanString ss str =
  let rec loop acc = function
  | ('"'::cs) -> Ok ((List.rev acc |> seqToStr), cs)
  | (c::cs) -> loop (c::acc) cs
  | [] -> Error {Line=ss.Line; Msg="Unterminated String"}
  match loop [] (str |> List.ofSeq) with
  | Ok (str', rest) -> 
      Ok ({ss 
            with Tokens = {
              Type=STRING str'
              Lexeme=sprintf "\"%s\"" str'
              Line=ss.Line }::ss.Tokens 
          }, rest)
  | Error e -> Error e

let private scanIdentifier ss str =
  let rec loop acc = function
  | (c::cs) when isAlphaNumeric c -> loop (c::acc) cs
  | cs -> ((List.rev acc |> seqToStr), cs)
  let (ident, rest) = loop [] str
  match Map.tryFind ident keywords with
  | Some keyword -> 
    ({ss with Tokens = { Type=keyword; Lexeme=ident; Line=ss.Line }::ss.Tokens }, rest)
  | None -> 
    ({ss with Tokens = { Type=IDENTIFIER ident; Lexeme=ident; Line=ss.Line}::ss.Tokens }, rest)

let private scanNumber ss str = 
  let rec loop frac acc = function
  | (c::cs) when isDigit c -> loop frac (c::acc) cs
  | (c::cs) when c = '.' && frac -> Ok ((List.rev acc |> seqToStr), cs)
  | (c::cs) when c = '.' -> loop true (c::acc) cs
  | s -> Ok ((List.rev acc |> seqToStr), s)
  loop false [] str
  |> Result.map 
    (fun (str', rest) -> 
      let newToken =  {Type=NUMBER (double str'); Lexeme=str'; Line=ss.Line}
      let tokens = newToken :: ss.Tokens
      ({ss with Tokens = tokens},rest))

let scan source =
  let rec loop ss src =
    match src with 
    | [] -> Ok ss
    | (c::cs) -> 
      match c with
      | '(' -> loop (addTokenC c LEFT_PAREN ss) cs
      | ')' -> loop (addTokenC c RIGHT_PAREN ss) cs
      | '{' -> loop (addTokenC c LEFT_BRACE ss) cs
      | '}' -> loop (addTokenC c RIGHT_BRACE ss) cs
      | ',' -> loop (addTokenC c COMMA ss) cs
      | '.' -> loop (addTokenC c DOT ss) cs
      | '-' -> loop (addTokenC c MINUS ss) cs
      | '+' -> loop (addTokenC c PLUS ss) cs
      | ';' -> loop (addTokenC c SEMICOLON ss) cs
      | '*' -> loop (addTokenC c STAR ss) cs

      // ignore whitespace, for new line, increment line in state
      | ' '
      | '\r'
      | '\t' -> loop ss cs
      | '\n' -> loop {ss with Line = ss.Line + 1} cs
      | '"'  -> scanString ss cs |> Result.bind (uncurry loop) 

      // two letter lexemes
      | _ -> 
        match c::cs with
        | [] -> Ok ss
        | ('/'::'/'::rest) -> loop {ss with Line = ss.Line + 1} (skipRemainingLine rest)
        | ('!'::'='::rest) -> loop (addToken "!=" BANG_EQUAL ss) rest
        | ('!'::rest)      -> loop (addToken "!" BANG ss) rest
        | ('>'::'='::rest) -> loop (addToken ">=" GREATER_EQUAL ss) rest
        | ('>'::rest)      -> loop (addToken ">" GREATER ss) rest
        | ('<'::'='::rest) -> loop (addToken "<=" LESS_EQUAL ss) rest
        | ('<'::rest)      -> loop (addToken "<" LESS ss) rest
        | ('='::'='::rest) -> loop (addToken "==" EQUAL_EQUAL ss) rest
        | ('='::rest)      -> loop (addToken "=" EQUAL ss) rest
        | (c::rest) when isDigit c -> 
          scanNumber ss (c::rest) 
          |> Result.bind (uncurry loop)
        | (c::rest) when isAlpha c -> 
          scanIdentifier ss (c::rest) |> (uncurry loop)
        | _ -> Error { Line = ss.Line; Msg = "Unexpected Character" }

  loop { Tokens = []; Line = 1 } (Seq.toList source)
  |> Result.map (fun ss -> List.rev ss.Tokens)