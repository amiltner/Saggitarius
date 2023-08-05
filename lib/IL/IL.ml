open MyStdLib
module IntToNtsRegex = DictOf (IntModule) (PairOf (Nts) (ConcretizedRegex))

type t =
  IndexedProduction.t list
  * Constraint.t list
  * IntToNtsRegex.t
  * ProcedPrefs.t list
[@@deriving eq, hash, ord]

let showlist (l : string list) : string =
  let rec sl x =
    match x with
    | [] -> "[]"
    | [hd] -> hd ^ "\n]"
    | hd :: tl -> hd ^ "\n" ^ sl tl
  in
  "[\n" ^ sl l
;;

(*From the induction language*)
let to_grammar ((productions, _, _, _) : t) : ClunkyGrammar.GrammarSpec.t =
  let ip_to_kvp (ip : IndexedProduction.t) : NumberedId.t * Production.t =
    NumberedId.create ip.pid ip.head, ip.symbols
  in
  let kvps = List.map ~f:ip_to_kvp productions in
  ClunkyGrammar.GrammarSpec.from_kvp_list kvps
;;

let show_constraints ((_, cs, _, _) : t) : string =
  let sl = List.map ~f:Constraint.show cs in
  showlist sl
;;

let show_grammar (il : t) : string =
  let g = to_grammar il in
  ClunkyGrammar.GrammarSpec.show g
;;

let show (il : t) : string =
  "Grammar: " ^ show_grammar il ^ "\n\n Constraints: " ^ show_constraints il
;;
