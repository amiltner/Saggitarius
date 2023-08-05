open MyStdLib
open Constraint.Z

type weight = float [@@deriving eq, hash, ord, show]
type increment = float
type ruleList = int list [@@deriving eq, hash, ord, show]

type t =
  | Parsimony of ruleList * weight (* Parsimony Score is  w * nRules/(nUsed+1) *)
  | Pref of ruleList * weight (*Everything in this list is individually prefered*)
  | AntiPref of ruleList * weight (*Everything in this list is individually NOT prefered*)
  | Bundle of ruleList * weight (* Preferred if you can have ALL of the following*)
  | SATForm of Constraint.t * weight (* Counts if a SAT formula is satisfied *)
[@@deriving eq, hash, ord, show]

let from_pref
    (env : (Id.t * int) list)
    (d : (Nts.t * int) list)
    (pm : (Nts.t * int list) list)
    (p : Preferences.t)
    : t
  =
  match p with
  | UParsimony (iid_ll, w) ->
    let is = List.concat_map ~f:(Exp.iid_list_to_int_list env d pm) iid_ll in
    Parsimony (is, w)
  | UPref (iid_ll, w) ->
    let is = List.concat_map ~f:(Exp.iid_list_to_int_list env d pm) iid_ll in
    Pref (is, w)
  | UAntiPref (iid_ll, w) ->
    let is = List.concat_map ~f:(Exp.iid_list_to_int_list env d pm) iid_ll in
    AntiPref (is, w)
  | UBundle (iid_ll, w) ->
    let is = List.concat_map ~f:(Exp.iid_list_to_int_list env d pm) iid_ll in
    Bundle (is, w)
  | USATForm (be, w) ->
    let c = BoolExp.to_constraint env d pm be in
    SATForm (c, w)
;;

(* Smart Constructors *)
let ascHierarchy idxsList w incr : t list =
  let process_single count idxs =
    let c = float_of_int count in
    Pref (idxs, w +. (c *. incr))
  in
  List.mapi ~f:process_single idxsList
;;

let indicators_of_ruleList (rl : ruleList) =
  let to_sym i = T.intify T.(!(Symbol.declare Bool (string_of_int i))) in
  List.map ~f:to_sym rl
;;

let rec to_objective (x : t) : [> znum ] term =
  match x with
  | Parsimony (allRules, w) ->
    let count = T.add (T.int 1 :: indicators_of_ruleList allRules) in
    let total = T.int (List.count ~f:(fun _ -> true) allRules) in
    let frac = T.div total count in
    T.mul [T.rat (Q.of_float w); frac]
  | Pref (idxs, w) ->
    if List.length idxs = 0
    then T.rat (Q.of_float 0.0)
    else (
      let indicators = indicators_of_ruleList idxs in
      let s = T.add indicators in
      T.mul [T.rat (Q.of_float w); s])
  | AntiPref (idxs, w) -> to_objective (Pref (idxs, -.Float.abs w))
  | Bundle (idxs, w) ->
    let symbols =
      List.map ~f:(fun i -> T.(!(Symbol.declare Bool (string_of_int i)))) idxs
    in
    T.mul [T.rat (Q.of_float w); T.intify (T.and_ symbols)]
  | SATForm (c, w) ->
    let boolean_formula = Constraint.to_SAT c in
    T.ite boolean_formula (T.rat (Q.of_float w)) (T.rat (Q.of_float 0.0))
;;

open ClunkyGrammar

let pref_parsimony (w : weight) (g : GrammarSpec.t) : t =
  let allRules : int list = GrammarSpec.filter_keys (fun _ -> true) g in
  Parsimony (allRules, w)
;;

let pref_from_nts (nts : Nts.t) (w : weight) (g : GrammarSpec.t) : t =
  let il = GrammarSpec.filter_by_nts nts g in
  Pref (il, w)
;;

let pref_from_name (name : Id.t) (w : weight) (g : GrammarSpec.t) =
  let il = GrammarSpec.filter_by_name name g in
  Pref (il, w)
;;

let aHier_from_names
    (names : Id.t list)
    (w : weight)
    (incr : increment)
    (g : GrammarSpec.t)
    : t list
  =
  let idxslist = List.map ~f:(fun n -> GrammarSpec.filter_by_name n g) names in
  ascHierarchy idxslist w incr
;;

let rec from_pholder
    (env : (Id.t * int) list)
    (d : (Nts.t * int) list)
    (pm : (Nts.t * int list) list)
    (p : Pholder.t)
    : t list
  =
  match p with
  | Singleton p -> [from_pref env d pm p]
  | PrefBinding ((i, r), p) ->
    let i1, i2 = Exp.eval_range env r in
    List.fold
      ~f:(fun acc v ->
        let env = (i, v) :: env in
        let res = from_pholder env d pm p in
        res @ acc)
      ~init:[]
      (range i1 i2)
;;
