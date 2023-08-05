S -> ??{i in [0,infty)} (? CSV{i} named CSVS{i}).

CSV{i in [0,infty)} ->
  ? Header{i} "\n" Rows{i}
  ? Rows{i}.

FieldSep ->
  ? "," named CommaSep
  ? "|" named BarSep
  ? ";" named SemiSep.

constraint (if CommaSep then 1 else 0) + (if BarSep then 1 else 0) + (if BarSep then 1 else 0) = 1.

QuoteMark -> ? "\"".

Header{i in [0,infty)} ->
  ? if (0 = i) then StringField
  ? if (0 < i) then StringField FieldSep Header{i-1}.

Rows{i in [0,infty)} ->
  ? Row{i} "\n" Rows{i} | Row{i}.

Row{i in [0,infty)} ->
  ? if (0 = i) then Field{i}
  ? if (0 < i) then Field{i} FieldSep Row{i-1}.

Empty ->
  ? "".

Field{i in [0,infty)} ->
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

constraint (if SlashDateSep then 1 else 0) + (if DotDateSep then 1 else 0) + (if DashDateSep then 1 else 0) + (if SpaceDateSep then 1 else 0) = 1.

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

positive_examples
     {"idx,word,email\n0,hello,arnold.mong@gmail.com\n1000,cruel,among@princeton.edu\n14123,world,helloworld@fake.org"}

     (* {"Date,Open,High,Low,Close,Volume\n2010-11-12,12.41,12.61,12.41,12.47,14686500\n2010-11-15,12.59,12.67,12.48,12.49,12628100\n2010-11-16,12.37,12.56,12.29,12.34,17121600\n2010-11-17,12.36,12.50,12.22,12.26,14793200\n2010-11-18,12.44,12.72,12.38,12.52,21840500\n2010-11-19,12.48,12.53,12.37,12.41,16923000\n2010-11-22,12.35,12.61,12.34,12.48,15045100\n2010-11-23,12.35,12.45,12.30,12.36,11613000\n2010-11-24,12.48,12.71,12.42,12.54,11024600\n2010-11-26,12.52,12.65,12.41,12.48,4026200"}*)

negative_examples
     {}
(*
     {"c,c\nc,c\nc,c\nc,c",
     "c,c,c,c\nc,c,c,c"} *)
