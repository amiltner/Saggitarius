open MyStdLib
open Exp

type t = bool_exp [@@deriving eq, hash, ord, show]
