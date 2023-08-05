S ->
  ??{i in [1,9)} (? CSV{i} named CSVS{i}).

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
          ? "\%"
          ? "*"
          ? "&"
          ? "#".

Quote -> ? "\'" named QuoteSingleQuote
         ? "\"" named QuoteDoubleQuote.

preference pref 1.0 (|productions(Escape)| = 0).
          
constraint(|productions(Quote)| = 1 | |productions(Quote)| < 1).


CSV{i in [1,9)} -> Rows{i}. (*Number of rows in the CSV*)

(*[i][j] is ROW i COL j*)

Rows{i in [1,9)} ->
  ? if (1 = i) then RowOfFixedLength{i}
  ? if (1 < i) then RowOfFixedLength{i} "\n" Rows{i-1}.

RowOfFixedLength{i in [1,9)} ->
  ??{j in [1,9)} (?Cell{i}{j} named Length{i}{j}).

Cell{i in [1,9)}{j in [1,9)}-> 
  ? if (1 = j) then Field{i}{j}
  ? if (1 < j) then Cell{i}{j-1} FieldSep Field{i}{j}.

Field{i in [1,9)}{j in [1,9)} ->
  ? Parsable | Quote Parsable Quote named Parsable{i}{j}
  ? Junk named Junk{i}{j}.
      

(*Type Score*)
preference {i in [1,9)}{j in [1,9)} antipref 1.0 [Junk{i}{j}].

(*Pattern Score*)
preference {j in [1,9)} pref -10.0 (|[[ Length{i}{j} | {i in [0,9)}]]| > 0).



(* Starting to populate types. *)

Bool -> ["0"-"1"].

(** words **)
Letter -> ["a"-"z"] | ["A"-"Z"].

Word -> Letter | Letter Word.

(** numbers **)
SingleInt -> ["0"-"9"].

MultipleInt -> SingleInt | SingleInt MultipleInt.

SignedInt -> MultipleInt | "+" MultipleInt | "-" MultipleInt.

(** alphanumeric (no spaces) **)
AlphaNumeric -> SingleInt | Letter | SingleInt AlphaNumeric | Letter AlphaNumeric.

ANSpaceChar -> SingleInt | Letter | " ".

ANSpace -> ANSpaceChar | ANSpaceChar ANSpace.

StringField -> ANSpace.

IntField -> MultipleInt.

(**** Web Stuff Terminal URLs and Emails ****)
InternetTerminal -> ".com"| ".net"| ".org"| ".edu".

URLReservedChar -> "!"| "*"| "'"| "("| ")"| ";"| ":"| "@"| "&"|"="| "+"|
  "$"| "|"| "/"| "?"| "#"| "["| "]"| "%".

URLUnreservedChar -> AlphaNumeric| "-"| "_"| "."| "~".

URLChar -> URLReservedChar | URLUnreservedChar.

URLChar -> AlphaNumeric | "-"| "_"| "."| "~"| "!"| "*"| "'"| "("| ")"| ";"| ":"| "@"| "&"|"="| "+"| "$"| "|"| "/"| "?"| "#"| "["| "]"| "%".

URLWord -> URLChar | URLChar URLWord.

URL -> URLWord InternetTerminal.

Email -> URLWord "@" URLWord InternetTerminal.

(**** Numbers (more than just integers) ****)
RadixPoint ->
  ? "." named DotRadix
  ? "," named CommaRadix.
constraint(|productions(RadixPoint)| = 1).

ThousandsSep ->
  ? "." named DotThousands
  ? "," named CommaThousands.
constraint(|productions(ThousandsSep)| = 1).
constraint(DotRadix => not DotThousands).
constraint(CommaRadix => not CommaThousands).

TSepNum -> MultipleInt | MultipleInt ThousandsSep TSepNum.

UnsignedNum1 -> TSepNum | TSepNum RadixPoint MultipleInt.

NumType1 -> UnsignedNum1 | "+" UnsignedNum1 | "-" UnsignedNum1.

ESuffix -> "e" SignedInt | "E" SignedInt. 

SuffixlessNum2 -> SignedInt | SignedInt RadixPoint MultipleInt.

NumType2 -> SuffixlessNum2 | SuffixlessNum2 ESuffix.

FancyNumber -> NumType1 | NumType2.

(**** Time ****)
Time -> 
    SingleInt SingleInt ":" SingleInt SingleInt ":" SingleInt SingleInt | (*HH:MM:SS*)
    SingleInt SingleInt ":" SingleInt SingleInt | (*HH:MM*) 
    SingleInt ":" SingleInt SingleInt (*H:MM*).

(**** Percentage ****)
Percentage -> FancyNumber "%".

(**** Currency ****)
(* need to process other currencies eventually probably*)
Currency -> "$" FancyNumber.

(**** N/A ****)
NonApplicable ->
  ? "n/a" | "N/A".

(**** Date ****)
DateSep ->
  ? "/"
  ? "."
  ? "-"
  ? " ".
constraint(|productions(DateSep)| = 1).

Year -> SingleInt SingleInt | SingleInt SingleInt SingleInt SingleInt.

MorD -> SingleInt | SingleInt SingleInt.

Date -> Year DateSep MorD DateSep MorD | MorD DateSep MorD DateSep Year.
(*need to include asian character dates maybe*)

(**** DateTime ****)
DateTime -> Date " " Time | Date "T" Time.

Empty -> "".

Parsable -> 
  Empty 
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
.

Junk ->
? CJunk named COMMAJUNK
? BJunk named BARJUNK
? SJunk named SEMIJUNK
? TJunk named TABJUNK.



Junk -> Quote QJunk Quote.
QJunk -> (NonDelimiterLetter | ";" | "|" | "," | "\t") QJunk | (NonDelimiterLetter | ";" | "|" | "," | "\t").

constraint(COMMASEP => COMMAJUNK).
constraint(BARSEP => BARJUNK).
constraint(SEMISEP => SEMIJUNK).
constraint(TABSEP => TABJUNK).


NonDelimiterLetter -> AlphaNumeric | "-"| "_"| "."| "~"| "!"| "*"| "("| ")"| ":"| "@"| "&"|"="| "+"| "$"| "/"| "?"| "#"| "["| "]"| "%" | "'" | " " | Escape Quote.
constraint(|productions(Junk)| < 3).

CJunk -> ((NonDelimiterLetter | ";" | "|" | "\t") CJunk) | (NonDelimiterLetter | ";" | "|" | "\t").
BJunk -> ((NonDelimiterLetter | ";" | "," | "\t") BJunk) | (NonDelimiterLetter | ";" | "," | "\t").
SJunk -> ((NonDelimiterLetter | "|" | "," | "\t") SJunk) | (NonDelimiterLetter | "|" | "," | "\t").
TJunk -> ((NonDelimiterLetter | "|" | "," | ";") TJunk) | (NonDelimiterLetter | "|" | "," | ";").


start S


positive_examples
 {"0,a\n1,b\n1,c"}
negative_examples
  {}
