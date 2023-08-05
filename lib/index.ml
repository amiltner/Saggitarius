open MyStdLib

type t =
  | Plus of t * t
  | Minus of t * t
  | Var of Id.t
  | Int of int
[@@deriving eq, hash, ord, show]

let destruct_int_exn (r : t) : int =
  match r with
  | Int i -> i
  | _ -> failwith "bad destruct"
;;

let rec eval (env : (Id.t * int) list) (i : t) : int =
  match i with
  | Plus (i1, i2) -> eval env i1 + eval env i2
  | Minus (i1, i2) -> eval env i1 - eval env i2
  | Var id -> List.Assoc.find_exn ~equal:Id.equal env id
  | Int i -> i
;;
