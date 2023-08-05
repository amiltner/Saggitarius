import "../floating_points/floats.ex"

CSV ->
  ??{i in [0,infty)} (? Table{i}).

Table{i in [0,infty)} ->
  ? Header{i} Newline Rows{i} named WithHeader{i}.

preference {i in [0,infty)} antipref 1.0 [WithHeader{i}].
preference {i in [0,infty)} antipref 2.0 [NoHeader{i}].

Header{i in [0,infty)} ->
  ? if (0 = i) then TrueArb
  ? if (0 < i) then Header{i-1} FieldSep TrueArb.

Rows{i in [0,infty)} ->
  Row{i} | (Row{i} Newline Rows{i}) | Row{i} Newline.

Row{i in [0,infty)} ->
  ? if (0 = i) then Column{i}
  ? if (0 < i) then Row{i-1} FieldSep Column{i}.

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

Newline -> "\n" | "\r\n".

Column{i in [0,infty)} ->
  ? FullString named FStr{i}
  ? Float named FFloat{i}
  ? QuotedInt named FQInt{i}
  ? QuotedFloat named FQFloat{i}
  ? Integer named FInt{i}
  ? Bool named FBool{i}
  ? FullAny named FAny{i}.

constraint (&&{i in [0,infty)} (|productions(Column{i})| = 1)).

preference {i in [0,infty)} pref 5.0 [FBool{i}].
preference {i in [0,infty)} pref 3.0 [FInt{i};FQInt{i}].
preference {i in [0,infty)} pref 2.0 [FFloat{i};FQFloat{i}].
preference {i in [0,infty)} pref 1.0 [FStr{i}].
preference {i in [0,infty)} antipref 10.0 [FAny{i}].
preference {i in [0,infty)} antipref 30.0 [FTarb{i}].

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

AnyChar -> ANChar | "{" | "}" | "-"| "_"| "."| "~"| "!"| "*"| "("| ")"| ":"| "@"| "&"|"="| "+"| "$"| "/"| "?"| "#"| "["| "]"| "%" | " " | "£" | slash FieldSep | NotQuote.
Any -> AnyChar | Any AnyChar.
QAnyChar -> AnyChar | "|" | "," | ";" | "’" | "\t" | Newline | Escape Quote.
QAny -> QAnyChar | QAny QAnyChar.

start CSV

