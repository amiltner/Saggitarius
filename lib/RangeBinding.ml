open MyStdLib
open Exp

type t = range_binding [@@deriving eq, hash, ord, show]

let replace_infty_with = range_binding_replace_infty_with
let size = size_range_binding
