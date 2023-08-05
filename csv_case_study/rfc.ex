import "../benchmarks/processed/floating_points/floats.ex"

CSV ->
  ??{i in [0,300)} (? Table{i}).

Table{i in [0,300)} ->
  ? Header{i} Newline Rows{i} named WithHeader{i}.

preference {i in [0,300)} antipref 1.0 [WithHeader{i}].
preference {i in [0,300)} antipref 2.0 [NoHeader{i}].

Header{i in [0,300)} ->
  ? if (0 = i) then TrueArb
  ? if (0 < i) then TrueArb FieldSep Header{i-1}.

Rows{i in [0,300)} ->
  Row{i} | (Row{i} Newline Rows{i}) | Row{i} Newline.

Row{i in [0,300)} ->
  ? if (0 = i) then Column{i}
  ? if (0 < i) then Column{i} FieldSep Row{i-1}.

FieldSep -> ",".
Escape -> "\"".
Quote -> "\"".

constraint(QuoteDoubleQuote => NotQuoteSingleQuote).
constraint(QuoteSingleQuote => NotQuoteDoubleQuote).
preference pref 1.0 (QuoteDoubleQuote).

constraint(|productions(Escape)| = 0 | |productions(Escape)| = 1).
preference pref 1.0 (|productions(Escape)| = 0).
constraint(|productions(Quote)| = 0 | |productions(Quote)| = 1).
preference pref 2.0 (|productions(Quote)| = 0).

Newline -> "\n" | "\r\n".

Column{i in [0,300)} ->
  ? FullString named FStr{i}
  ? Float named FFloat{i}
  ? QuotedInt named FQInt{i}
  ? QuotedFloat named FQFloat{i}
  ? Integer named FInt{i}
  ? Bool named FBool{i}
  ? FullAny named FAny{i}.

constraint (&&{i in [0,300)} (|productions(Column{i})| = 1)).

preference {i in [0,300)} pref 5.0 [FBool{i}].
preference {i in [0,300)} pref 3.0 [FInt{i};FQInt{i}].
preference {i in [0,300)} pref 2.0 [FFloat{i};FQFloat{i}].
preference {i in [0,300)} pref 1.0 [FStr{i}].
preference {i in [0,300)} antipref 10.0 [FAny{i}].
preference {i in [0,300)} antipref 30.0 [FTarb{i}].

TrueArb -> FullAny | FullString.

FullAny -> Any | Empty.

QuotedInt -> Quote Integer Quote.

QuotedFloat -> Quote Float Quote.

FullString ->
  ? Quote QAny Quote
  ? Quote Quote.

Empty -> "".

Bool ->
  ? "0" | "1" | Int
  ? "true" | "false"
  ? "True" | "False".

Letter -> ["a"-"z"] | ["A"-"Z"].
ANChar -> Letter | Digit.
AlphaNumeric -> ANChar | AlphaNumeric ANChar.

AnyChar -> ANChar | "{" | "}" | "-"| "_"| "."| "~"| "!"| "*"| "("| ")"| ":"| "@"| "&"|"="| "+"| "$"| "/"| "?"| "#"| "["| "]"| "%" | " " | ">" | "<" | "'" | "`" | "|" | ";" | "\t".
Any -> AnyChar | Any AnyChar.
QAnyChar -> AnyChar | FieldSep | Newline | Escape Quote.
QAny -> QAnyChar | QAny QAnyChar.

start CSV

