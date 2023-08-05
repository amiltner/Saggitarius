open MyStdLib

type t = Char.t list [@@deriving eq, hash, ord]

let rec show x : string =
  match x with
  | [] -> ""
  | hd :: tl -> Char.escaped hd ^ " " ^ show tl
;;

let pp (fmt : Format.formatter) (x : t) = Format.pp_print_string fmt (show x)
