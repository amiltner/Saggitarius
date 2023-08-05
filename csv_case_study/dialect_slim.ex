S ->
  ??{i in [1,7)} (? CSV{i} named CSVS{i}).
constraint (|productions(S)| = 1).

FieldSep -> 
? "," named COMMASEP 
? "|" named BARSEP
? "\t" named TABSEP
? ":"
? ";" named SEMISEP.
constraint(|productions(FieldSep)| < 2).

Escape -> ? slash
          ? "#".

Quote -> ? "'" named QuoteSingleQuote
         ? "\"" named QuoteDoubleQuote.
NotQuote -> ? "'" named NotQuoteSingleQuote
         ? "\"" named NotQuoteDoubleQuote.
constraint(QuoteDoubleQuote => NotQuoteSingleQuote).
constraint(QuoteSingleQuote => NotQuoteDoubleQuote).
preference pref 1.0 (QuoteDoubleQuote).
preference pref 2.0 (|productions(Quote)| = 0).


constraint(|productions(Escape)| = 0 | |productions(Escape)| = 1).
preference pref 1.0 (|productions(Escape)| = 0).
constraint(|productions(Quote)| = 0 | |productions(Quote)| = 1).

CSV{i in [1,7)} -> Rows{i}. (*Number of rows in the CSV*)
constraint &&{i in [1,7)} (|productions(CSV{i})| = 1).

(*[i][j] is ROW i COL j*)

Newline -> "\n" | "\r\n".
Rows{i in [1,7)} ->
  ? if (1 = i) then RowOfFixedLength{i}
  ? if (1 < i) then RowOfFixedLength{i} Newline Rows{i-1}.


RowOfFixedLength{i in [1,7)} ->
  ??{j in [1,25)} (?Cell{i}{j} named Length{i}{j}).

preference {j in [1,25)} pref -10.0 (|[[ Length{i}{j} | {i in [0,7)}]]| > 0).

constraint &&{i in [1,7)} (|productions(RowOfFixedLength{i})| = 1).

Cell{i in [1,7)}{j in [1,25)}-> 
  ? if (1 = j) then Field{i}{j}
  ? if (1 < j) then Field{i}{j} FieldSep Cell{i}{j-1}.

Field{i in [1,7)}{j in [1,25)} ->
  ? Quote QAny Quote named QAnyCell{i}{j}
  ? Quote Quote
  ? Empty
  ? Any named AnyCell{i}{j}
  ? Satanic named SatanicCell{i}{j}.

constraint &&{i in [1,7)}(&&{j in [1,25)} (|productions(Field{i}{j})| = 1)).
preference {i in [1,7)}{j in [1,25)} pref 3.0 (QAnyCell{i}{j}).
preference {i in [1,7)}{j in [1,25)} pref 2.0 (AnyCell{i}{j}).
preference {i in [1,7)}{j in [1,25)} pref -100.0 (SatanicCell{i}{j}).

(*Currently no difference.*)
Empty -> "".

Digit -> ["0"-"9"].
Letter -> ["a"-"z"] | ["A"-"Z"].
ANChar -> Letter | Digit.
AlphaNumeric -> ANChar | ANChar AlphaNumeric.

AnyChar -> ANChar | "{" | "}" | "-"| "_"| "."| "~"| "!"| "*"| "("| ")"| ":"| "@"| "&"|"="| "+"| "$"| "/"| "?"| "#"| "["| "]"| "%" | " " | "£" | "^" | ">" | "<".
AnyChar -> NotQuote.
AnyChar -> slash FieldSep. 
AnyChar -> ? NotQuote.
Any -> AnyChar | AnyChar Any.
QAnyChar -> AnyChar | "|" | "," | ";" | "’" | ":" | "\t". 
QAnyChar -> Newline.
QAnyChar -> Escape Quote.
QAny -> QAnyChar | QAny QAnyChar.
SatanicChar -> AnyChar | Quote | "," | ";" | ":" | "|". 
Satanic -> SatanicChar | SatanicChar Satanic.

start S

(*
positive_examples
 {"\n\n\na,b,c\n,,a,e,,b\na\n\n"}
negative_examples
 {} *)
