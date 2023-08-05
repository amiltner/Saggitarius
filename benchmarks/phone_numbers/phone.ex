S ->
  ? CountryCode CountryBreak AreaCode ABreak1 ExchangeCode ABreak2 StationCode (*3 part local numbers*) named ThreePart
  ? CountryCode CountryBreak AreaCode ABreak1 LongStationCode (*2 part local numbers*) named TwoPart.

preference antipref 1.0 [ThreePart].
preference antipref 3.0 [TwoPart].

Int ->
  ? ["0"-"9"].

CountryCode ->
  ? "" 
  ? Int
  ? Int Int
  ? Int Int Int
  ? "+" Int
  ? "+" Int Int
  ? "+" Int Int Int.

preference antipref 1.0 productions (CountryCode).

CountryBreak ->
  ? ""
  ? " "
  ? "-".

preference antipref 1.0 productions (CountryBreak).

AreaCode ->
  ? (Int Int)
  ? (Int Int Int)
  ? (Int Int Int Int)
  ? "(" (Int Int) ")"
  ? "(" (Int Int Int) ")"
  ? "(" (Int Int Int Int) ")"
  ? ("0" Int)
  ? ("0" Int Int)
  ? ("0" Int Int Int)
  ? "(" ("0" Int) ")"
  ? "(" ("0" Int Int) ")"
  ? "(" ("0" Int Int Int) ")".

preference antipref 1.0 productions (AreaCode).

ABreak1 ->
  ? ""
  ? " "
  ? "-".

preference antipref 1.0 productions (ABreak1).

ABreak2 ->
  ? ""
  ? " "
  ? "-".

preference antipref 1.0 productions (ABreak2).

ExchangeCode ->
  ? Int Int
  ? Int Int Int
  ? Int Int Int Int
  .

preference antipref 1.0 productions (ExchangeCode).

StationCode ->
  ? Int Int
  ? Int Int Int
  ? Int Int Int Int
  ? Int Int Int Int Int
  .

preference antipref 1.0 productions (StationCode).

LongStationCode ->
  ? Int Int Int Int
  ? Int Int Int Int Int
  ? Int Int Int Int Int Int
  ? Int Int Int Int Int Int Int
  ? Int Int Int Int Int Int Int Int
  .

preference antipref 1.0 productions (LongStationCode).


start S
