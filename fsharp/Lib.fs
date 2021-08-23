module Flox.Lib

/// <summary>
/// Convert a sequence of characters (could be a list) to a string explicitly 
/// </summary>
let seqToStr (cs:char seq) = System.String.Concat(cs)

/// <summary>
/// Convert a string to a list of chars - useful in that it causes type coersion of its
/// parameter to a string. See the implementation of scan below
/// </summary>
let strToList (s:string) = Seq.toList s

let isAlpha c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  c = '_';

let isDigit c =
  c >= '0' && c <= '9';

let isAlphaNumeric c =
  isAlpha(c) || isDigit(c);

let uncurry f (x,y) = f x y

let curry f x y = f(x,y) 

let sbAppend (s:string) (sb:System.Text.StringBuilder) =
  sb.Append(s)