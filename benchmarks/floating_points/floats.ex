Float ->
  ? Textual
  ? SciNot.

SciNot ->
  ? ("-" | "") Nonzero ("" | ("." Digit Digit*)) "E" SciInt.

SciInt ->
  ? "0" | (SciSign Nonzero Digit* )
  ? "0" | (SciSign Digit* ).

Textual ->
  ? AboveOne
  ? BelowOne.

Decimals ->
  ? "." Digit*
  ? "".

SciSign ->
  ? "-" | ""
  ? "-" | "+".

Integer ->
  ? "0" | (("" | "-") Nonzero Digit*).

AboveOne ->
  ? ("" | "-") Nonzero Digit* Decimals.

BelowOne ->
  ? ("" | "-") "0" Decimals
  ? ("" | "-") Decimals.

Digit ->
  ? ["0"-"9"].

Nonzero ->
  ? ["1"-"9"].

start Float
