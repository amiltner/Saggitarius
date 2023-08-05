S ->
  ? BasicName
  ? Salutation " " BasicName
  ? BasicName " " PostName
  ? Salutation " " BasicName " " PostName.

PostName ->
  ? "Junior"
  ? "Senior"
  ? "Sr."
  ? "Jr."
  ? "I"
  ? "II"
  ? "III"
  ? "IV"
  ? "V"
  ? "VI"
  ? "VII"
  ? "VIII"
  ? "IX"
  ? "X".

BasicName ->
  ??{i in [0,infty)} (? NameCount{i}).

NameCount{i in [0,infty)} ->
  ? if (0 = i) then SingleName
  ? if (0 < i) then SingleName " " NameCount{i-1}.

SingleName ->
  ? Upper Lower Lower*.

Salutation ->
  ? "Mr."
  ? "Ms."
  ? "Mrs."
  ? "Sir"
  ? "Dr.".

SalutationN ->
  ? Salutation BasicName.

Upper ->
  ? ["A"-"Z"].

Lower ->
  ? ["a"-"z"].

BasicSingle ->
  ? Upper Lower (Lower* ).

start S
