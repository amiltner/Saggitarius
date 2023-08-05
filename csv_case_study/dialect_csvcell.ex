S ->
  ??{i in [1,50)} (? CSV{i} named CSVS{i}).
constraint (|productions(S)| = 1).

FieldSep -> 
? "," named COMMASEP 
? "|" named BARSEP
? "\t" named TABSEP
? ";" named SEMISEP.
constraint(|productions(FieldSep)| = 1).

Escape -> ? "!"
          ? "?"
          ? "\""
          ? "\'"
          ? "."
          ? ","
          ? ";"
          ? ":"
          ? "%"
          ? "*"
          ? "&"
          ? "#".

Quote -> ? "\'" named QuoteSingleQuote
         ? "\"" named QuoteDoubleQuote.

constraint(|productions(Escape)| = 0 | |productions(Escape)| = 1).
preference pref 1.0 (|productions(Escape)| = 0).
constraint(|productions(Quote)| = 0 | |productions(Quote)| = 1).
preference pref 1.0 (|productions(Quote)| = 0).

CSV{i in [1,50)} -> Rows{i}. (*Number of rows in the CSV*)
constraint &&{i in [1,50)} (|productions(CSV{i})| = 1).

(*[i][j] is ROW i COL j*)

Newline -> "\n" | "\r\n".
SatanicEndingNewlines -> Newline | Newline SatanicEndingNewlines.
Rows{i in [1,50)} ->
  ? if (1 = i) then RowOfFixedLength{i} | RowOfFixedLength{i} SatanicEndingNewlines
  ? if (1 < i) then RowOfFixedLength{i} Newline Rows{i-1}.

RowOfFixedLength{i in [1,50)} ->
  ??{j in [1,50)} (?Cell{i}{j} named Length{i}{j}).

constraint &&{i in [1,50)} (|productions(RowOfFixedLength{i})| = 1).

Cell{i in [1,50)}{j in [1,50)}-> 
  ? if (1 = j) then Field{i}{j}
  ? if (1 < j) then Cell{i}{j-1} FieldSep Field{i}{j}.

Field{i in [1,50)}{j in [1,50)} ->
  ? Quote QSimple Quote named QSimpleCell{i}{j}
  ? Simple named SimpleCell{i}{j}
  ? Quote QAny Quote named QAnyCell{i}{j}
  ? Any named AnyCell{i}{j}.

preference {i in [1,50)}{j in [1,50)} pref 5.0 (QSimpleCell{i}{j}).
preference {i in [1,50)}{j in [1,50)} pref 4.0 (SimpleCell{i}{j}).
preference {i in [1,50)}{j in [1,50)} pref 3.0 (QAnyCell{i}{j}).
preference {i in [1,50)}{j in [1,50)} pref 2.0 (AnyCell{i}{j}).
constraint &&{i in [1,50)} (&&{j in [1,50)} (|productions(Field{i}{j})| = 1)).

(*Currently no difference.*)
Simple -> Empty | AlphaNumeric.
Empty -> "".

QSimple -> Simple.

Digit -> ["0"-"9"].
Letter -> ["a"-"z"] | ["A"-"Z"].
ANChar -> Letter | Digit.
AlphaNumeric -> ANChar | AlphaNumeric ANChar.

AnyChar -> ANChar | "-"| "_"| "."| "~"| "!"| "*"| "("| ")"| ":"| "@"| "&"|"="| "+"| "$"| "/"| "?"| "#"| "["| "]"| "%" | "'" | " ".
Any -> AnyChar | Any AnyChar.
QAnyChar -> AnyChar | "|" | "," | ";" | "\t" | Escape Quote.
QAny -> QAnyChar | QAny QAnyChar.

start S
(* 
positive_examples
 {"a,b,c\n,a,e,,b\na"}
negative_examples
 {} *)
