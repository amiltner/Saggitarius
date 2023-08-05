open MyStdLib

type t =
  | True
  | False
  | EQIndex of Index.t * Index.t
  | LTIndex of Index.t * Index.t
  | Not of t
  | And of t * t
  | Or of t * t
[@@deriving eq, hash, ord, show]

let rec eval (g : t) (env : (Id.t * int) list) : bool =
  match g with
  | True -> true
  | False -> false
  | EQIndex (i1, i2) -> Int.equal (Index.eval env i1) (Index.eval env i2)
  | LTIndex (i1, i2) -> Index.eval env i1 < Index.eval env i2
  | Not g -> not (eval g env)
  | And (g1, g2) -> eval g1 env && eval g2 env
  | Or (g1, g2) -> eval g1 env || eval g2 env
;;
