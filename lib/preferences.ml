open MyStdLib
open Constraint.Z

type weight = float [@@deriving eq, hash, ord, show]
type increment = float
type ruleList = Exp.iid_ll [@@deriving eq, hash, ord, show]

type t =
  | UParsimony of ruleList * weight (* Parsimony Score is  w * nRules/(nUsed+1) *)
  | UPref of ruleList * weight (*Everything in this list is individually prefered*)
  | UAntiPref of ruleList * weight (*Everything in this list is individually NOT prefered*)
  | UBundle of ruleList * weight (* Preferred if you can have ALL of the following*)
  | USATForm of Exp.bool_exp * weight (* Counts if a SAT formula is satisfied *)
[@@deriving eq, hash, ord, show]

let size (p : t) : int =
  match p with
  | UParsimony (rl, _) ->
    let rs = Exp.size_iid_ll rl in
    2 + rs
  | UPref (rl, _) ->
    let rs = Exp.size_iid_ll rl in
    2 + rs
  | UAntiPref (rl, _) ->
    let rs = Exp.size_iid_ll rl in
    2 + rs
  | UBundle (rl, _) ->
    let rs = Exp.size_iid_ll rl in
    2 + rs
  | USATForm (rl, _) ->
    let rs = Exp.size_bool_exp rl in
    1 + rs
;;

let replace_infty_with (i : int) (p : t) : t =
  match p with
  | UParsimony (rl, w) -> UParsimony (Exp.iid_ll_replace_infty_with i rl, w)
  | UPref (rl, w) -> UPref (Exp.iid_ll_replace_infty_with i rl, w)
  | UAntiPref (rl, w) -> UAntiPref (Exp.iid_ll_replace_infty_with i rl, w)
  | UBundle (rl, w) -> UBundle (Exp.iid_ll_replace_infty_with i rl, w)
  | USATForm (c, w) -> USATForm (BoolExp.replace_infty_with i c, w)
;;

let rec replace_range_with
    (i:Id.t)
    (lr:int)
    (hr:int)
    (p:t)
  : t =
  match p with
  | UParsimony (rl, w) ->
    UParsimony (Exp.iid_ll_replace_range_with i lr hr rl, w)
  | UPref (rl, w) -> UPref (Exp.iid_ll_replace_range_with i lr hr rl, w)
  | UAntiPref (rl, w) -> UAntiPref (Exp.iid_ll_replace_range_with i lr hr rl, w)
  | UBundle (rl, w) -> UBundle (Exp.iid_ll_replace_range_with i lr hr rl, w)
  | USATForm (c, w) -> USATForm (Exp.bool_exp_replace_range_with i lr hr c, w)
;;
