StreetName ->
  ? Direction " " BasicName " " PostName
  ? BasicName " " PostName.

PostName ->
  ? PostNameShort
  ? PostNameLong
  ? PostNameShort " " Direction
  ? PostNameLong " " Direction.

Direction ->
  ? LongDirection
  ? ShortDirection.

LongDirection ->
  ? "North"
  ? "South"
  ? "East"
  ? "West".

ShortDirection ->
  ? "N."
  ? "S."
  ? "E."
  ? "W.".

PostNameShort ->
  ? "St."
  ? "Circ."
  ? "Rd."
  ? "Ln."
  ? "Ct."
  ? "Ave."
  ? "Blvd."
  ? "Dr.".

PostNameLong ->
  ? "Street"
  ? "Circle"
  ? "Road"
  ? "Lane"
  ? "Court"
  ? "Avenue"
  ? "Drive".

BasicName ->
  ? SingleName (" " SingleName)*.

NameCount{i in [0,5)} ->
  ? if (0 = i) then SingleName
  ? if (0 < i) then SingleName " " NameCount{i-1}.

Digit ->
  ? ["0"-"9"].

SingleName ->
  ? Upper Lower*
  ? Digit Digit* Lower*.

Upper ->
  ? ["A"-"Z"].

Lower ->
  ? ["a"-"z"].

start StreetName
