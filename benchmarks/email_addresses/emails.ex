S ->
  ? Rep "@" Domain.

Rep ->
  ? (["a"-"z"] | ["A"-"Z"] | "." | "-" | "_" | ["0"-"9"])*.

ArbDomainRep ->
  ? (["a"-"z"] | ["A"-"Z"] | "-" | "_" | ["0"-"9"])*.

SpecDomainRepComponent{i in [0,8)}{j in [0,16)} ->
  ? "a" ? "b" ? "c" ? "d" ? "e" ? "f" ? "g" ? "h" ? "i" ? "j" ? "k" ? "l"
  ? "m" ? "n" ? "o" ? "p" ? "q" ? "r" ? "s" ? "t" ? "u" ? "v" ? "w" ? "x"
  ? "y" ? "z"
  ? "A" ? "B" ? "C" ? "D" ? "E" ? "F" ? "G" ? "H" ? "I" ? "J" ? "K" ? "L"
  ? "M" ? "N" ? "O" ? "P" ? "Q" ? "R" ? "S" ? "T" ? "U" ? "V" ? "W" ? "X"
  ? "Y" ? "Z" ? "0" ? "1" ? "2" ? "3" ? "4" ? "5" ? "6" ? "7" ? "8" ? "9"
  ? "-" ? "_" .

SpecDomainRep{i in [0,8)}{j in [0,16)} ->
  ? SpecDomainRepComponent{i}{j}
  ? SpecDomainRepComponent{i}{j} SpecDomainRep{i}{j-1}.

TLDomain ->
  ? MinChoice named MC
  ? IncludeAll named IA.

Domain ->
  ? (ArbDomainRep ".")* TLDomain named ArbNum
  ? FixedDomain TLDomain named FixedNum
  ? UpToDomainCount TLDomain named UpTo
  ? AtLeastDomainCount TLDomain named AtLeast.

preference antipref 1.0 [ArbNum].
preference antipref 3.0 [FixedNum].
preference antipref 5.0 [UpTo].
preference antipref 7.0 [AtLeast].

FixedDomain ->
  ??{i in [0,8)} (? DomainExact{i}).

preference antipref 1.0 productions(FixedDomain).

DomainExact{i in [0,8)} ->
  ? if (0 = i) then SpecDomainRep{i}{15} "." named DE{i}
  ? if (0 < i) then SpecDomainRep{i}{15} "." DomainExact{i-1} named DEC{i}.

preference antipref 2.0 [[DE{i} | {i in [0,8]}]].
preference antipref 2.0 [[DEC{i} | {i in [0,8]}]].

UpToDomainCount ->
  ??{i in [0,8)} (? DomainCountAtLeast{i}).

preference antipref 2.0 productions(UpToDomainCount).

DomainCountAtLeast{i in [0,8)} ->
  ? (ArbDomainRep ".")*
  ? DomainCountAtLeast{i-1} (ArbDomainRep ".")*.

DomainCountAtMost{i in [0,8)} ->
  ? ArbDomainRep "."
  ? if (0 < i) then ArbDomainRep "." DomainCountAtMost{i-1}.

preference antipref 1.0 [MC].
preference pref 1.0 [IA].

IncludeAll ->
  ? "com" | "org" | "net" | "int" | "edu" | "gov" | "mil"
  | "apra" | ".ac" | ".ad" | ".ae" | ".af" | "ag" | "ai" | "al"
  | "am" | "ninja".

MinChoice ->
  ? "com" ? "org" ? "net" ? "int" ? "edu" ? "gov" ? "mil"
    ? "apra" ? ".ac" ? ".ad" ? ".ae" ? ".af" ? "ag" ? "ai" ? "al"
    ? "am" ? "ninja".

preference antipref 1.0 productions(MinChoice).

start S
