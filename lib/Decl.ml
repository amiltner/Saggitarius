open MyStdLib

type t =
  | MProd of MProd.t
  | Constraint of BExp.t
[@@deriving eq, hash, ord, show, variants]
