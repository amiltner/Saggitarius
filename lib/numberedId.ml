open MyStdLib

type t =
  { idx : int
  ; id : Nts.t [@ignore]
  }
[@@deriving bin_io, eq, hash, ord, sexp, show]

let create (i : int) (s : Nts.t) : t = { idx = i; id = s }
let equal (a : t) (b : t) : bool = a.idx = b.idx
let show x : string = "(" ^ string_of_int x.idx ^ "," ^ Nts.show x.id ^ ")"
let pp (fmt : Format.formatter) (x : t) = Format.pp_print_string fmt (show x)
let match_name (x : t) (s : string) : bool = String.equal (Nts.show x.id) s
let string_id (x : t) : string = string_of_int x.idx
