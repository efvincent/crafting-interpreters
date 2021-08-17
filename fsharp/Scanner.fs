module Flox.Scanner

open Flox.Types

let keywords = 
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
  Source: string
  Start: int
  CurIdx: int
  Tokens: Token list
  Line: int
}

let private isAtEnd s =
  s.CurIdx >= s.Source.Length

let private addToken state t = 
  let lexeme = state.Source.[state.Start..state.CurIdx-1]
  Ok { state 
        with 
        Start = state.CurIdx
        Tokens = { Type = t; Lexeme = lexeme; Line = state.Line } :: state.Tokens }

let private addIfMatch state expected tt tf =
  if isAtEnd state 
  then addToken state tf
  elif state.Source.[state.CurIdx] <> expected 
  then addToken state tf
  else 
    let state' = 
      { state 
          with
          Start = state.CurIdx + 1 
          CurIdx = state.CurIdx + 1 }
    addToken state' tt 

let private peek ss =
  if isAtEnd ss 
  then None 
  else Some ss.Source.[ss.CurIdx]

let private peekNext ss =
  if ss.CurIdx + 1 >= ss.Source.Length 
  then None 
  else Some ss.Source.[ss.CurIdx + 1]

let private advance ss = 
  (ss.Source.[ss.CurIdx], {ss with CurIdx = ss.CurIdx + 1 })

let private isAlpha c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  c = '_';

let private isDigit c =
  c >= '0' && c <= '9';

let private isAlphaNumeric c =
  isAlpha(c) || isDigit(c);

let private scanIdentifier ss = 
  let addT state =
    let text = state.Source.[state.Start..state.CurIdx-1]
    match Map.tryFind text keywords with
    | Some tt -> addToken state tt
    | None -> addToken state (IDENTIFIER text)
  let rec loop state =
    match peek state with
    | Some c when isAlphaNumeric c -> 
        let ( _, state' ) = advance state
        loop state'
    | Some _ 
    | None -> 
      addT state
  loop ss

let scanNumber ss =
  // consume all digits from the current index
  let rec scanDigits state =
    match peek state with
    | Some c when isDigit c ->
      let (_,state') = advance state
      scanDigits state'
    | Some _ 
    | None -> state

  let state = 
    let wholePart = scanDigits ss
    match peek wholePart with
    | Some '.' -> 
      // if we stopped scanning digits because we found a decimal, 
      // check if the character after the decimal is a digit, and if it
      // is we have a fraction part. Scan that part as well.
      match peekNext wholePart with
      | Some c when isDigit c ->
        let (_, fraction) = advance wholePart
        scanDigits fraction
      | Some _ 
      | None -> wholePart
    | Some _ 
    | None -> wholePart
  
  let rawNum = state.Source.[state.Start .. state.CurIdx-1]
  match System.Double.TryParse(rawNum) with 
  | (true, n) -> addToken state (NUMBER n)
  | (false, _) -> Error { Msg = sprintf "Could not parse '%s' as a number" rawNum; Line=state.Line }

let private scanString ss =
  let rec loop state =
    match peek state with
    | Some c when c <> '"' && not (isAtEnd state) ->
      let state' = 
        if c = '\n' then { state with Line = state.Line + 1 }
        else state
      let (_, advanced) = advance state'
      loop advanced
    | Some _ 
    | None ->
      if isAtEnd state then
        Error { Line = state.Line; Msg = "Unterminated staring" }
      else
        let (_, state') = advance state
        let value = state'.Source.[(state'.Start + 1) .. (state'.CurIdx - 2)]
        addToken state' (STRING value)
  loop ss

let private scanToEOL ss =
  let rec loop state =
    let (c, state') = advance state
    if isAtEnd state' then state' 
    elif c = '\n' then { state' with Line = state'.Line + 1 }
    else loop state'
  loop ss

let private scanToken ss =
  
  let (c,state) = advance {ss with Start = ss.CurIdx}
  match c with
  | '(' -> addToken state LEFT_PAREN
  | ')' -> addToken state RIGHT_PAREN
  | '{' -> addToken state LEFT_BRACE
  | '}' -> addToken state RIGHT_BRACE
  | ',' -> addToken state COMMA
  | '.' -> addToken state DOT
  | '-' -> addToken state MINUS
  | '+' -> addToken state PLUS
  | ';' -> addToken state SEMICOLON
  | '*' -> addToken state STAR
  | '!' -> addIfMatch state '=' BANG_EQUAL BANG
  | '=' -> addIfMatch state '=' EQUAL_EQUAL EQUAL
  | '<' -> addIfMatch state '=' LESS_EQUAL LESS
  | '>' -> addIfMatch state '=' GREATER_EQUAL GREATER
  | '/' -> 
    if peek state = Some '/' then
      // comments run to the end of the line, advance that far
      // and create no token
      Ok <| scanToEOL state
    else
      addToken state SLASH

  | ' '
  | '\r'
  | '\t' -> Ok state    // ignore whitespace

  | '\n' -> Ok { state with Line = state.Line + 1 }

  | '"' -> scanString state

  | _ -> 
    if isDigit c 
    then scanNumber state
    elif isAlpha c 
    then scanIdentifier state
    else
      Error { 
        Line = state.Line 
        Msg = "Unexpected Character" }

let scan source =
  let ss = {
    Line = 1
    Source = source
    Start = 0
    CurIdx = 0
    Tokens = []
  }
  let rec loop state =
    if isAtEnd state 
    then List.rev state.Tokens |> Ok
    else
      match scanToken {state with Start = state.CurIdx} with 
      | Ok state' -> loop state'
      | Error e -> Error e
  loop ss
