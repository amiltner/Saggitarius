S ->
  ??{i in [1,10)} (? CSV{i} named CSVS{i}).

CSV{i in [1,10)} ->
  ??{j in [1,30)}
    (? Header{i} "\n" Rows{i}{j}
     ? Rows{i}{j}).

Header{i in [1,10)} ->
  ? if (1 = i) then StringField
  ? if (1 < i) then StringField FieldSep Header{i-1}.

Rows{i in [1,10)}{j in [1,30)} ->
  ? if (1 = j) then Row{i}{j}
  ? if (1 < j) then Row{i}{j} "\n" Rows{i}{j-1}.

Row{i in [1,10)}{j in [1,30)} ->
  ? if (1 = i) then Field{i}{j}
  ? if (1 < i) then Field{i}{j} FieldSep Row{i-1}{j}.

Field{i in [1,10)}{j in [1,30)} ->
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
  ? "0" | "1" | "2" | "3" | "4".

StringField ->
  ? "a" | "b" | "c" | "d" | "e" | "f".

start S

positive_examples
{"c,c\n0,a\n1,b\n1,c"}
negative_examples
{""}
