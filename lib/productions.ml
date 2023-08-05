open MyStdLib

type t = Production.t list [@@deriving eq, hash, ord, show]
