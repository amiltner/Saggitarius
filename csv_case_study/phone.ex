S ->
  ? American.

Int ->
  ? ["0"-"9"].

CountryCode ->
  ? "" 
  ? Int (*american domestic*)
  ? Int Int Int (*american international*)
  ? "+" Int
  ? "+" Int Int (*american international2*).

CountryBreak ->
  ? "" named EPS
  ? " " named SPACE
  ? "-" named DASH.

constraint (if EPS then 1 else 0) + (if SPACE then 1 else 0) + (if DASH then 1 else 0) = 1.

AreaCode ->
  ? Int Int Int named NOPAREN
  ? "(" Int Int Int ")" named PAREN.

constraint (if NOPAREN then 1 else 0) + (if PAREN then 1 else 0) = 1.

ABreak1 ->
  ? "" named EPSAB
  ? " " named SPACEAB
  ? "-" named DASHAB.

constraint (if EPSAB then 1 else 0) + (if SPACEAB then 1 else 0) + (if DASHAB then 1 else 0) = 1.

ABreak2 ->
  ? "" named EPSAB2
  ? " " named SPACEAB2
  ? "-" named DASHAB2.

constraint (if EPSAB2 then 1 else 0) + (if SPACEAB2 then 1 else 0) + (if DASHAB2 then 1 else 0) = 1.

American ->
  ? CountryCode CountryBreak AreaCode ABreak1 Int Int Int ABreak2 Int Int Int Int.


start S

positive_examples
{"+12404230593",
 "+443019722851"}
negative_examples
{"2404230593"}
