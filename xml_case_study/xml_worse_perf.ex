Digit ->
  ? ["0"-"9"].

PickableNoWSChar{i in [0,8)}{j in [0,8)} ->
  ? "a" ? "b" ? "c" ? "d" ? "e" ? "f" ? "g" ? "h" ? "i" ? "j" ? "k" ? "l"
  ? "m" ? "n" ? "o" ? "p" ? "q" ? "r" ? "s" ? "t" ? "u" ? "v" ? "w" ? "x"
  ? "y" ? "z"
  ? "A" ? "B" ? "C" ? "D" ? "E" ? "F" ? "G" ? "H" ? "I" ? "J" ? "K" ? "L"
  ? "M" ? "N" ? "O" ? "P" ? "Q" ? "R" ? "S" ? "T" ? "U" ? "V" ? "W" ? "X"
  ? "Y" ? "Z" ? "0" ? "1" ? "2" ? "3" ? "4" ? "5" ? "6" ? "7" ? "8" ? "9"
  ? "-" ? "_" .
constraint (&&{i in [0,8)} (&&{j in [0,8)} (|productions(PickableNoWSChar{i}{j})| = 1))).

NoWSChar ->
  ? (["a"-"z"] | ["A"-"Z"] | "." | "-" | "_" | ["0"-"9"]).

NoWSString ->
  ? NoWSChar*.

PickableNoWSStr{i in [0,8)}{j in [0,8)} ->
  ? PickableNoWSChar{i}{j}
  ? PickableNoWSChar{i}{j} PickableNoWSStr{i}{j+1}.
constraint (&&{i in [0,8)} (&&{j in [0,8)} (|productions(PickableNoWSStr{i}{j})| = 1))).

PickableNoWSString{i in [0,8)} ->
  ? PickableNoWSStr{i}{0}.

ArbitraryChar ->
  ? (["a"-"z"] | ["A"-"Z"] | "." | "-" | "_" | ["0"-"9"] | " " | "\n").

FullText ->
  ? ArbitraryChar FullText
  ? ArbitraryChar.

Attribute ->
  ? WS NoWSString "=\"" FullText "\"".

Attributes ->
  ? Attribute Attributes
  ? WS .

StartTag{i in [0,8)} ->
  ? ("<" PickableNoWSString{i} Attributes ">").

EndTag{i in [0,8)} ->
  ? ("</" PickableNoWSString{i} ">").

SelfClosingTag{i in [0,8)} ->
  ? ("<" PickableNoWSString{i} Attributes "/>").

WSC ->
  ? (" " | "\n").

WS ->
  ? ("" | (WSC WS)).

NodeChoice{i in [0,8)} ->
  ??{j in [0,8)} (? Node{j}).
preference{i in [0,8)} antipref 5.0 productions(NodeChoice{i}).

NodeChoices{i in [0,8)} ->
  ? NodeChoice{i}
  ? NodeChoice{i} NodeChoices{i}.
preference{i in [0,8)} antipref 5.0 productions(NodeChoices{i}).

NodeInternal{i in [0,8)} ->
  ? FullText
  ? WS
  ? NodeChoices{i}.
preference{i in [0,8)} antipref 5.0 productions(NodeInternal{i}).

Node{i in [0,8)} ->
  ? (WS SelfClosingTag{i} WS)
  ? (WS StartTag{i} WS NodeInternal{i} EndTag{i} WS).
preference{i in [0,8)} antipref 5.0 productions(Node{i}).

S ->
  ??{i in [0,8)} (? Node{i}).

start S
