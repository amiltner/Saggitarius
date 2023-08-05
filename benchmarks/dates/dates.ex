S ->
  ? DOW (", " | " ") Date
  ? Date.

DOW ->
  ? ("Sunday" | "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday" | "Saturday").

Date ->
  ? YearAtEnd named YAE
  ? YearAtStart named YAS.

preference antipref 1.0 [YAE].
preference antipref 10.0 [YAS].

Digit ->
  ? ["0"-"9"].

YearAtEnd ->
  ? Month Sep{0} Day Sep{1} Year named DL
  ? Month Sep{2} Day Sep{3} TYear named DLT
  ? Day Sep{4} Month Sep{5} Year named DF
  ? Day Sep{6} Month Sep{7} TYear named DFT.

YearAtStart ->
  ? Year Sep{8} Month Sep{9} Day
  ? TYear Sep{10} Month Sep{11} Day
  ? Year Sep{12} Day Sep{13} Month
  ? TYear Sep{14} Day Sep{15} Month.

Sep{i in [0,15]} ->
  ? " "
  ? "."
  ? "-"
  ? "/".

Month ->
  ? NumMonth
  ? NumMonthFixed
  ? WrittenMonth
  ? ShortWMonth
  ? ShortWMonth "."
  ? WrittenMonth "," named WMComma.

constraint WMComma => YAE.
constraint WMComma => (DF | DFT).

NumMonthFixed ->
  ? ( "01"
    | "02"
    | "03"
    | "04"
    | "05"
    | "06"
    | "07"
    | "08"
    | "09"
    | "10"
    | "11"
    | "12").

NumMonth ->
  ? ( "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9"
    | "10"
    | "11"
    | "12").

WrittenMonth ->
  ? ( "January"
    | "February"
    | "March"
    | "April"
    | "May"
    | "June"
    | "July"
    | "August"
    | "September"
    | "October"
    | "November"
    | "December").

ShortWMonth ->
  ? ( "Jan"
    | "Feb"
    | "Mar"
    | "Apr"
    | "May"
    | "Jun"
    | "Jul"
    | "Aug"
    | "Sep"
    | "Oct"
    | "Nov"
    | "Dec").

Year ->
  ? Digit Digit Digit Digit named FourYL
  ? (Digit)* named ArbYL.

TYear ->
  ? Digit Digit named TwoYL.

preference antipref 2.0 [TwoYL].
preference antipref 1.0 [FourYL].
preference antipref 5.0 [ArbYL].

Day ->
  ? FixedDay
  ? SimpleDay
  ? SimpleDay "," named DC.

constraint DC => YAE.
constraint DC => (DL | DLT).

FixedDay ->
  ? ( "01" | "02" | "03" | "04" | "05" | "06" | "07" | "08" | "09" | "10"
    | "11" | "12" | "13" | "14" | "15" | "16" | "17" | "18" | "19" | "20"
    | "21" | "22" | "23" | "24" | "25" | "26" | "27" | "28" | "29" | "30"
    | "31").

SimpleDay ->
  ? ( "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "10"
    | "11" | "12" | "13" | "14" | "15" | "16" | "17" | "18" | "19" | "20"
    | "21" | "22" | "23" | "24" | "25" | "26" | "27" | "28" | "29" | "30"
    | "31").

start S
