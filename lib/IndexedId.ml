open MyStdLib

type t = Exp.indexed_id [@@deriving eq, hash, ord, show]

let replace_infty_with = Exp.indexed_id_replace_infty_with
let contains_var = Exp.indexed_id_contains_var
let size = Exp.size_indexed_id
