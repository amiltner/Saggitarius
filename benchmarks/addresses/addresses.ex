import "../street_names/street_names.ex"
import "../states/state.ex"

Digit ->
  ? ["0"-"9"].

Int ->
  ? Digit Digit*.

CityName ->
  ? ["A"-"Z"] ["a"-"z"]*.

City ->
  ? CityName (" " CityName)*.

Zip ->
  ? Digit Digit Digit Digit Digit.

ZipWS ->
  ? " "
  ? "  ".

S ->
  ? Int " " StreetName "
" City ", " State ZipWS Zip.

start S
