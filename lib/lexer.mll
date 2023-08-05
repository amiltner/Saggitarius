{
open Core
open Lexing
open Parser

exception Lexer_error of string

let reserved_words : (string * Parser.token) list =
  [ ("start", START)
  ; ("positive_examples", POSITIVE_EXAMPLES)
  ; ("negative_examples", NEGATIVE_EXAMPLES)
  ; ("then", THEN)
  ; ("sum_over", SumOver)
  ; ("else", ELSE)
  ; ("constraint", CONSTRAINT)
  ; ("preference", PREFERENCE)
  ; ("emit", EMIT)
  ; ("pref", PREF)
  ; ("str", TOSTR)
  ; ("where", WHERE)
  ; ("import", IMPORT)
  ; ("antipref", ANTIPREF)
  ; ("productions", PRODUCTIONS)
  ; ("parsimony", PARSIMONY)
  ; ("bundle", BUNDLE)
  ; ("by", BY)
  ; ("of", OF)
  ; ("options", OPTIONS)
  ; ("symbolic", SYMBOLIC)
  ; ("infty", INFTY)
  ; ("slash", SLASH)
  ; ("if", IF)
  ; ("iff", IFF)
  ; ("in", IN)
  ; ("let", LET)
  ; ("true", TRUE)
  ; ("false", FALSE)
  ; ("from", FROM)
  ; ("not", NOT)
  ; ("for", FOR)
  ; ("named", NAMED)
  ; ("nat", NAT)
  ]

let symbols : (string * Parser.token) list =
  [ ("->", ARR)
  ; ("=>", FATARR)
  ; (",", COMMA)
  ; (";", SEMI)
  ; ("@", AT)
  ; (".", DOT)
  ; ("??", QQ)
  ; ("?", QMARK)
  ; ("{", LBRACE)
  ; ("}", RBRACE)
  ; ("[", LBRACKET)
  ; ("]", RBRACKET)
  ; ("*", STAR)
  ; ("&", AMP)
  ; ("<", LT)
  ; (">", GT)
  ; ("=", EQ)
  ; ("(", LPAREN)
  ; (")", RPAREN)
  ; ("+", PLUS)
  ; ("-", DASH)
  ; ("|", BAR)
  ]

let create_token lexbuf =
  let str = lexeme lexbuf in
  match List.Assoc.find ~equal:String.equal reserved_words str with
  | None   -> LID str
  | Some t -> t

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  match List.Assoc.find ~equal:String.equal symbols str with
  | None   -> raise @@ Lexer_error ("Unexpected token: " ^ str)
  | Some t -> t

let remove_quotes lexbuf =
  let str = lexeme lexbuf in
  let len = String.length str in
  String.sub str ~pos:1 ~len:(len-2)
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let lowercase  = ['a'-'z']
let uppercase  = ['A'-'Z']
let character  = uppercase | lowercase
let digit      = ['0'-'9']
let string = ('"''"') | ('"' ("\\\"" | [^'"'])* '"')

rule token = parse
  | eof   { EOF }
  | '-'? digit+ '.' digit+ { FLOAT (float_of_string (lexeme lexbuf)) }
  | digit+ { INT (int_of_string (lexeme lexbuf)) }
  (*| "#" digit+ { create_proj lexbuf } *)
  | "(*" {comments 0 lexbuf}
  | whitespace+ | newline+    { token lexbuf }
  | lowercase (digit | character | '_')* { create_token lexbuf }
  | uppercase (digit | character | '_')* { UID (lexeme lexbuf) }
  | "??" | '?' | "|>" | '=' | "->" | "=>" | '*' | ',' | ':' | ';' | '|' | '(' | ')'
  | '{' | '}' | '[' | ']' | '_' | '.' | '<' | '>' | '-' | '+' | '&'
    { create_symbol lexbuf }
  | string { STR (remove_quotes lexbuf) }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }

and comments level = parse
  | "*)" { if level = 0 then token lexbuf
	   else comments (level-1) lexbuf }
  | "(*" { comments (level+1) lexbuf}
  | [^ '\n'] { comments level lexbuf }
  | "\n" { comments level lexbuf }
  | eof	 { failwith "Comments are not closed" }
