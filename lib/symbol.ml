open MyStdLib
open CharRange

type t =
  | Terminal of CRList.t
  | Nonterminal of Nts.t
[@@deriving bin_io, eq, hash, ord, sexp, show]

(* Printing Functions *)
let show x =
  match x with
  | Terminal c -> CRList.show c
  | Nonterminal nt -> Nts.show nt
;;

let pp (fmt : Format.formatter) (x : t) = Format.pp_print_string fmt (show x)

(*Terminal Functions*)
let mk_terminal (c : CRList.t) : t = Terminal c

let is_terminal (p : t) : bool =
  match p with
  | Terminal _ -> true
  | _ -> false
;;

(* Nonterminal Functions *)
let mk_nonterminal (s : Nts.t) : t = Nonterminal s

let apply_nonterminal (type a) (p : t) ~(f : Nts.t -> a) : a option =
  match p with
  | Nonterminal s -> Some (f s)
  | _ -> None
;;

let is_nonterminal (p : t) : bool =
  match p with
  | Nonterminal _ -> true
  | _ -> false
;;

let destruct_nonterminal : t -> Nts.t option = apply_nonterminal ~f:ident
