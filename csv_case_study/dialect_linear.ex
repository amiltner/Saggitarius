S ->
  ??{i in [1,10)} (? CSV{i} named CSVS{i}).
constraint (|productions(S)| = 1).

FieldSep -> 
? "," named COMMASEP 
? "|" named BARSEP
? "\t" named TABSEP
? ";" named SEMISEP.
constraint(|productions(FieldSep)| = 1).

Escape -> ? slash
          ? "#".

Quote -> ? "'" named QuoteSingleQuote
         ? "\"" named QuoteDoubleQuote.
NotQuote -> ? "'" named NotQuoteSingleQuote
         ? "\"" named NotQuoteDoubleQuote.
constraint(QuoteDoubleQuote => NotQuoteSingleQuote).
constraint(QuoteSingleQuote => NotQuoteDoubleQuote).
preference pref 1.0 (QuoteDoubleQuote).


constraint(|productions(Escape)| = 0 | |productions(Escape)| = 1).
preference pref 1.0 (|productions(Escape)| = 0).
constraint(|productions(Quote)| = 0 | |productions(Quote)| = 1).
preference pref 2.0 (|productions(Quote)| = 0).

CSV{i in [1,10)} -> Rows{i}.

Newline -> "\n" | "\r\n".
Rows{i in [1,10)} ->
  ? if (1 = i) then Row
  ? if (1 < i) then Row Newline Rows{i-1}.

Row-> 
  ? Field
  ? Row FieldSep Field.

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

AnyChar -> ANChar | "{" | "}" | "-"| "_"| "."| "~"| "!"| "*"| "("| ")"| ":"| "@"| "&"|"="| "+"| "$"| "/"| "?"| "#"| "["| "]"| "%" | " " | "£" | slash FieldSep | NotQuote.
Any -> AnyChar | Any AnyChar.
QAnyChar -> AnyChar | "|" | "," | ";" | "’" | "\t" | Newline | Escape Quote.
QAny -> QAnyChar | QAny QAnyChar.

start S

(*
positive_examples
 {"\n\n\na,b,c\n,,a,e,,b\na\n\n"}
negative_examples
 {} *)
