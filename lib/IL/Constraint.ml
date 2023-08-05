open MyStdLib

module Z = Z3overlay.Make (struct
  let ctx = Z3.mk_context []
end)

open Z

type t =
  | All of t list
  | And of t list
  | Or of t list
  | Not of t
  | ProdId of int
  | LessThan of t list * int
  | GreaterThan of t list * int
  | Eq of t list * int
  | Implies of t * t
  | Iff of t * t
[@@deriving eq, hash, ord, show, variants]

(* Smart Constructors *)
let directedPath (name : Nts.t) (idxs : int list list) : t list = []

let count_true (l : [> zbool ] term list) : [> zint ] term =
  T.add (List.map ~f:T.intify l)
;;

let rec to_SAT (c : t) : [> Z.zbool ] Z.term =
  match c with
  | All _ -> failwith "no exist"
  | And l ->
    let tl = List.map ~f:to_SAT l in
    T.and_ tl
  | Or l -> T.or_ (List.map ~f:to_SAT l)
  | Not t1 -> T.not (to_SAT t1)
  | ProdId i -> T.(!(Symbol.declare Bool (string_of_int i)))
  | LessThan (constrs, tv) ->
    let termConstrs = List.map ~f:to_SAT constrs in
    T.lt (count_true termConstrs) (T.int tv)
  | GreaterThan (constrs, tv) ->
    let termConstrs = List.map ~f:to_SAT constrs in
    T.gt (count_true termConstrs) (T.int tv)
  | Eq (constrs, tv) ->
    let termConstrs = List.map ~f:to_SAT constrs in
    T.eq (count_true termConstrs) (T.int tv)
  | Implies (t1, t2) ->
    let term1 = to_SAT t1 in
    let term2 = to_SAT t2 in
    T.imply term1 term2
  | Iff (t1, t2) ->
    let term1 = to_SAT t1 in
    let term2 = to_SAT t2 in
    T.iff term1 term2
;;

let showlist (l : string list) : string =
  let rec sl x =
    match x with
    | [] -> "[]"
    | [hd] -> hd ^ "]"
    | hd :: tl -> hd ^ "," ^ sl tl
  in
  "[" ^ sl l
;;

let rec show_t_list (l : t list) : string =
  let individuals = List.map ~f:show l in
  showlist individuals

and show (c : t) : string =
  match c with
  | All l -> "All(" ^ show_t_list l ^ ")"
  | And l -> "And(" ^ show_t_list l ^ ")"
  | Or l -> "Or(" ^ show_t_list l ^ ")"
  | Not x -> "Not(" ^ show x ^ ")"
  | ProdId pid -> string_of_int pid
  | LessThan (tl, i) -> "SumOf" ^ show_t_list tl ^ "<" ^ string_of_int i
  | GreaterThan (tl, i) -> "SumOf" ^ show_t_list tl ^ ">" ^ string_of_int i
  | Eq (tl, i) -> "SumOf" ^ show_t_list tl ^ "=" ^ string_of_int i
  | Implies (t1, t2) -> show t1 ^ "=>" ^ show t2
  | Iff (t1, t2) -> show t1 ^ "<=>" ^ show t2
;;
