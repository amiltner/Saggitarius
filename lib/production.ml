open MyStdLib

type t = Symbol.t list [@@deriving bin_io, eq, hash, ord, sexp, show]
(*Treat the first entry in the list as the head *)

let rec show x : string =
  match x with
  | [] -> ""
  | hd :: tl -> Symbol.show hd ^ " " ^ show tl
;;

let extract_ntss : t -> Nts.t list =
  List.filter_map ~f:Symbol.destruct_nonterminal
;;

let pp (fmt : Format.formatter) (x : t) = Format.pp_print_string fmt (show x)
