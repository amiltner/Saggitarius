S ->
  ? FullTime
  ? NoSec.

NoSec ->
  ? Hour ":" Minute " " AMPM{0}
  ? Hour ":" Minute named NoAMPMNoSec.

AMPM{i in [0,2)} ->
  ? "AM" | "PM"
  ? "am" | "pm".

FullTime ->
  ? Hour ":" Minute ":" Second " " AMPM{1}
  ? Hour ":" Minute ":" Second named NoAMPMFT.

OneToTwelve ->
  ? ("1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "10" | "11" | "12").

OneToTwelveZPrec ->
  ? ("01" | "02" | "03" | "04" | "05" | "06" | "07" | "08" | "09" | "10" | "11" | "12").

ZeroTo23ZPrec ->
  ? ("00" | "01" | "02" | "03" | "04" | "05" | "06" | "07" | "08" | "09" | "10" | "11" | "12"
   | "13" | "14" | "15" | "16" | "17" | "18" | "19" | "20" | "21" | "22" | "23").

ZeroTo23 ->
  ? ("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "10" | "11" | "12"
   | "13" | "14" | "15" | "16" | "17" | "18" | "19" | "20" | "21" | "22" | "23").

Hour ->
  ? OneToTwelve
  ? OneToTwelveZPrec
  ? ZeroTo23ZPrec
  ? ZeroTo23.

Minute ->
  ? ["0"-"5"] ["0"-"9"]
  ? ["0"-"9"] | (["1"-"5"] ["0"-"9"]).

Second ->
  ? ["0"-"5"] ["0"-"9"]
  ? ["0"-"9"] | (["1"-"5"] ["0"-"9"]).

start S
