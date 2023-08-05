include MyStdLib
include ClunkyGrammar
open Constraint.Z

let empty = T.true_

let gram_to_termslist (g : GrammarSpec.t) =
  let nids = GrammarSpec.key_list g in
  let identifiers = List.map ~f:NumberedId.string_id nids in
  List.map ~f:(fun ruleID -> T.(!(Symbol.declare Bool ruleID))) identifiers
;;

let init_from_IL ((productions, constraints, _, _) : IL.t)
    : [> Constraint.Z.zbool ] Constraint.Z.term
  =
  let get_mandatory_ids (ip : IndexedProduction.t) =
    if ip.required
    then Some T.(!(Symbol.declare Bool (IndexedProduction.to_string ip)))
    else None
  in
  let requiredVars = List.filter_map ~f:get_mandatory_ids productions in
  let satConstraints = List.map ~f:Constraint.to_SAT constraints in
  T.and_ (requiredVars @ satConstraints)
;;

let prov_to_termslist (prov : RuleSet.t) =
  let provIDs = RuleSet.as_list prov in
  let provStrings = List.map ~f:NumberedId.string_id provIDs in
  List.map ~f:(fun ruleID -> T.(!(Symbol.declare Bool ruleID))) provStrings
;;

(*For false positives 'we need to remove one of the rules we did use'*)
let add_why_prov (prov : RuleSet.t) (sat : [> zbool ] term) =
  let provTerms = prov_to_termslist prov in
  T.and_ [sat; T.not (T.and_ provTerms)]
;;

(*For false negatives 'we need to include one of the rules we didn't use'*)
let add_whynot_prov (prov : RuleSet.t) (sat : [> zbool ] term) =
  let provTerms = prov_to_termslist prov in
  T.and_ [sat; T.or_ provTerms]
;;

let solvable (sat : [> zbool ] term) : bool =
  let solver = Solver.make () in
  Solver.add ~solver sat;
  let result = Solver.check ~solver [] in
  match result with
  | Unsat _ -> false
  | Unkown _ -> false
  | Sat _ -> true
;;

let solve (sat : [> zbool ] term) : Z3.Model.model option =
  let solver = Solver.make () in
  Solver.add ~solver sat;
  let result = Solver.check ~solver [] in
  match result with
  | Unsat _ -> None
  | Unkown _ -> None
  | Sat (lazy model) -> Some model
;;

(* Tests to see if a used/unused split satisfies the basic requirements *)
let test_prov
    (requirements : [> zbool ] term)
    (used : RuleSet.t)
    (unused : RuleSet.t)
    : bool
  =
  let usedTerms = T.and_ (prov_to_termslist used) in
  let unusedTerms = T.not (T.or_ (prov_to_termslist unused)) in
  solvable (T.and_ [requirements; usedTerms; unusedTerms])
;;

let get_assignment (model : Z3.Model.model) (boolname : NumberedId.t) : bool =
  let s = Symbol.declare Bool (NumberedId.string_id boolname) in
  Model.get_value ~model s
;;

(*Gets all the variables with positive label*)
let get_positives
    (grammar : ClunkyGrammar.GrammarSpec.t)
    (model : Z3.Model.model)
    : NumberedId.t list
  =
  let filter_rule (kvp : NumberedId.t * Production.t) : bool =
    let nid, prod = kvp in
    let key = NumberedId.string_id nid in
    let s = Symbol.declare Bool key in
    Model.get_value ~model s
  in
  let positives =
    List.filter ~f:filter_rule (ClunkyGrammar.GrammarSpec.as_kvp_list grammar)
  in
  List.map ~f:fst positives
;;

(*Gets all the variables with negative label*)
let get_negatives
    (grammar : ClunkyGrammar.GrammarSpec.t)
    (model : Z3.Model.model)
    : NumberedId.t list
  =
  let filter_rule (kvp : NumberedId.t * Production.t) : bool =
    let nid, prod = kvp in
    let key = NumberedId.string_id nid in
    let s = Symbol.declare Bool key in
    not (Model.get_value ~model s)
  in
  let negatives =
    List.filter ~f:filter_rule (ClunkyGrammar.GrammarSpec.as_kvp_list grammar)
  in
  List.map ~f:fst negatives
;;
