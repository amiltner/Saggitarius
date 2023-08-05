S -> ??{i in [0,10)} (? CSV{i} named CSVS{i}).

CSV{i in [0,10)} ->
  ? Header{i} "\n" Rows{i}
  ? Rows{i}.

Header{i in [0,10)} ->
  ? if (0 = i) then StringField
  ? if (0 < i) then StringField FieldSep Header{i-1}.

Rows{i in [0,10)} ->
  ? Row{i} "\n" Rows{i} | Row{i}.

Row{i in [0,10)} ->
  ? if (0 = i) then Field{i}
  ? if (0 < i) then Field{i} FieldSep Row{i-1}.

Field{i in [0,10)} ->
  ? BoolField
  ? IntField
  ? StringField.

FieldSep ->
  ? ","
  ? "|"
  ? ";".

BoolField ->
  ? "0" | "1".

IntField ->
  ? ["0"-"9"].

StringField ->
  ? "a" | "b" | "c" | "d" | "e" | "f".

start S

positive_examples
{"c,c\n0,a\n1,b\n1,c"}
negative_examples
{"c,c\n0,1\n2,3\n1,0"}

