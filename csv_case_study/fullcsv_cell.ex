S ->
  ??{i in [1,10)} (? CSV{i} named CSVS{i}).

constraint sum_over{i in [1,10)} if CSVS{i} then 1 else 0 = 1.

CSV{i in [1,10)} ->
  ??{j in [1,30)}
    (? Header{i} "\n" Rows{i}{j}
     ? Rows{i}{j}).

Header{i in [1,10)} ->
  ? if (1 = i) then StringField
  ? if (1 < i) then StringField FieldSep Header{i-1}.

Rows{i in [1,10)}{j in [1,30)} ->
  ? if (1 = j) then Row{i}{j}
  ? if (1 < j) then Row{i}{j} "\n" Rows{i}{j-1}.

Row{i in [1,10)}{j in [1,30)} ->
  ? if (1 = i) then Field{i}{j}
  ? if (1 < i) then Field{i}{j} FieldSep Row{i-1}{j}.

Field{i in [1,10)}{j in [1,30)} ->
  ? Parsable named Parsable
  ? Junk named Junk.

Parsable -> 
  ? Empty 
| Email
| URL
| Bool
| FancyNumber
| Time
| Percentage
| Currency
| StringField
| NonApplicable
| DateTime
| Date
| UnrecognizedType
.

NonDelimiterLetter ->
? ["A"-"Z"] | ["a"-"z"] | "-"| "_"| "."| "~"| "!"| "*"| "'"| "("| ")"| ":"| "@"| "&"|"="| "+"|
"$"| "/"| "?"| "#"| "["| "]"| "%".

Junk ->
? CJunk named COMMAJUNK
? BJunk named BARJUNK
? SJunk named SEMIJUNK.

CJunk ->
  ? ((NonDelimiterLetter | ";" | "|") CJunk) | (NonDelimiterLetter | ";" | "|").
BJunk ->
  ? ((NonDelimiterLetter | ";" | ",") BJunk) | (NonDelimiterLetter | ";" | ",").
SJunk ->
  ? ((NonDelimiterLetter | "|" | ",") SJunk) | (NonDelimiterLetter | "|" | ",").

FieldSep ->
  ? "," named CommaSep
  ? "|" named BarSep
  ? ";" named SemiSep.

constraint (if CommaSep then 1 else 0) + (if BarSep then 1 else 0) + (if BarSep then 1 else 0) = 1.

QuoteMark -> ? "\"".

Header{i in [0,10)} ->
  ? if (0 = i) then StringField
  ? if (0 < i) then StringField FieldSep Header{i-1}.

Rows{i in [0,10)} ->
  ? Row{i} "\n" Rows{i} | Row{i}.

Row{i in [0,10)} ->
  ? if (0 = i) then Field{i}
  ? if (0 < i) then Field{i} FieldSep Row{i-1}.

Empty ->
  ? "".

Field{i in [0,10)} ->
  ? Empty
  ? Email
  ? Url
  ? Bool
  ? MultipleInt
  ? Time
  ? Percentage
  ? Currency
  ? StringField
  ? NonApplicable
  ? Date
  ? DateTime.

Bool ->
  ? ["0"-"1"].


(** words **)
LLetter ->
  ? ["a"-"z"].

ULetter ->
  ? ["A"-"Z"].

Letter ->
  ? LLetter | ULetter.

Word ->
  ? Letter | Letter Word.

(** numbers **)
SingleInt ->
  ? ["0"-"9"].

MultipleInt ->
  ? SingleInt | SingleInt MultipleInt.

SignedInt ->
  ? MultipleInt | "+" MultipleInt | "-" MultipleInt.

(** alphanumeric (no spaces) **)
AlphaNumeric ->
  ? SingleInt | Letter | SingleInt AlphaNumeric | Letter AlphaNumeric.

ANSpaceChar ->
  ? SingleInt | Letter | " ".

ANSpace ->
  ? ANSpaceChar | ANSpaceChar ANSpace.

StringField ->
  ? ANSpace.

IntField ->
  ? MultipleInt.



(**** Web Stuff Terminal URLs and Emails ****)
InternetTerminal ->
  ? ".com"| ".net"| ".org"| ".edu".

(*this can be optimized probably*)

(*
URLReservedChar ->
  ? "!"| "*"| "'"| "("| ")"| ";"| ":"| "@"| "&"|"="| "+"|
  "$"| "|"| "/"| "?"| "#"| "["| "]"| "%".

URLUnreservedChar ->
  ? AlphaNumeric| "-"| "_"| "."| "~".

URLChar ->
  ? URLReservedChar | URLUnreservedChar. *)

URLChar ->
  ? AlphaNumeric| "-"| "_"| "."| "~"| "!"| "*"| "'"| "("| ")"| ";"| ":"| "@"| "&"|"="| "+"|
  "$"| "|"| "/"| "?"| "#"| "["| "]"| "%".

URLWord ->
  ? URLChar | URLChar URLWord.

URL ->
  ? URLWord InternetTerminal.

Email ->
  ? URLWord "@" URLWord InternetTerminal.

(**** Numbers (more than just integers) ****)
RadixPoint ->
  ? "." named DotRadix
  ? "," named CommaRadix.

constraint (if DotRadix then 1 else 0) + (if CommaRadix then 1 else 0) = 1.

ThousandsSep ->
  ? "." named DotThousands
  ? "," named CommaThousands.
constraint (if DotThousands then 1 else 0) + (if CommaThousands then 1 else 0) = 1.

(*should include some way to say that these seperators have to be different... *)
TSepNum ->
  ? MultipleInt | MultipleInt ThousandsSep TSepNum.

UnsignedNum1 ->
  ? TSepNum | TSepNum RadixPoint MultipleInt.

NumType1 ->
  ? UnsignedNum1 | "+" UnsignedNum1 | "-" UnsignedNum1.

ESuffix ->
  ? "e" SignedInt | "E" SignedInt. 

SuffixlessNum2 ->
  ? SignedInt | SignedInt RadixPoint MultipleInt.

NumType2 ->
  ? SuffixlessNum2 | SuffixlessNum2 ESuffix.

FancyNumber ->
  ? NumType1 | NumType2.

(**** Time ****)
Time -> 
  ? SingleInt SingleInt ":" SingleInt SingleInt ":" SingleInt SingleInt | (*HH:MM:SS*)
    SingleInt SingleInt ":" SingleInt SingleInt | (*HH:MM*) 
    SingleInt ":" SingleInt SingleInt (*H:MM*).

(**** Percentage ****)
Percentage ->
  ? "%" FancyNumber.

(**** Currency ****)
(* need to process other currencies eventually probably*)
Currency ->
  ? "$" FancyNumber.

(**** N/A ****)
NonApplicable ->
  ? "n/a" | "N/A".

(**** Date ****)
DateSep ->
  ? "/" named SlashDateSep
  ? "." named DotDateSep
  ? "-" named DashDateSep
  ? " " named SpaceDateSep.

constraint(|Productions(DateSep)| = 1)

Year ->
  ? SingleInt SingleInt | SingleInt SingleInt SingleInt SingleInt.

MorD ->
  ? SingleInt | SingleInt SingleInt.

Date ->
  ? Year DateSep MorD DateSep MorD | MorD DateSep MorD DateSep Year.
(*need to include asian character dates maybe*)

(**** DateTime ****)
DateTime ->
  ? Date " " Time | Date "T" Time.

start S
