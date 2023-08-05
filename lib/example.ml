open MyStdLib

type t = Char.t list [@@deriving eq, hash, ord, show]
