S -> Rows.

FieldSep -> 
? "," named COMMASEP 
? "|" named BARSEP
? "\t" named TABSEP
? ":" named COLONSEP
? ";" named SEMISEP.
constraint(|productions(FieldSep)| < 2).

Escape -> ? slash
          ? "#".
constraint(|productions(Escape)| = 0 | |productions(Escape)| = 1).
preference pref 1.0 (|productions(Escape)| = 0).

Quote -> ? "'" named QuoteSingleQuote
         ? "\"" named QuoteDoubleQuote.
NotQuote -> ? "'" named NotQuoteSingleQuote
         ? "\"" named NotQuoteDoubleQuote.
constraint(QuoteDoubleQuote => NotQuoteSingleQuote).
constraint(QuoteSingleQuote => NotQuoteDoubleQuote).
preference pref 1.0 (QuoteDoubleQuote).


constraint(|productions(Quote)| = 0 | |productions(Quote)| = 1).
preference pref 2.0 (|productions(Quote)| = 0).

Newline -> "\n" | "\r\n".
Rows -> Row | Row Newline Rows.

Row-> 
  ? Field
  ? Field FieldSep Row.

Field->
  ? Quote QAny Quote
  ? Quote Quote
  ? Any
  ? Empty.


Empty -> "".

Digit -> ["0"-"9"].
Letter -> ["a"-"z"] | ["A"-"Z"].
ANChar -> Letter | Digit.
AlphaNumeric -> ANChar | AlphaNumeric ANChar.

AnyChar -> ANChar | "{" | "}" | "-"| "_"| "."| "~"| "!"| "*"| "("| ")"| ":"| "@"| "&"|"="| "+"| "$"| "/"| "?"| "#"| "["| "]"| "%" | " " | "£" | "^" | "<" | ">".
AnyChar -> slash FieldSep. 

Any -> AnyChar | Any AnyChar.
QAnyChar -> AnyChar | "|" | "," | ";" | "’" | "\t" | Newline | Escape Quote.
QAnyChar -> ? NotQuote.

QAny -> QAnyChar | QAny QAnyChar.

start S

(*
positive_examples
 {"\n\n\na,b,c\n,,a,e,,b\na\n\n"}
negative_examples
 {} *)
