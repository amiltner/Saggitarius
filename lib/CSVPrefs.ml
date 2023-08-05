open MyStdLib
open Core
open ClunkyGrammar

let pref_headers ?weight:(w = 1.0) (g : GrammarSpec.t) : ProcedPrefs.t =
  ProcedPrefs.pref_from_name (Id "Header") w g
;;

let parsing_hierarchy ?start:(w = 1.0) ?increment:(i = 1.0) (g : GrammarSpec.t)
    : ProcedPrefs.t list
  =
  let wrongway =
    [ "Empty"
    ; "Email"
    ; "URL"
    ; "Bool"
    ; "FancyNumber"
    ; "Time"
    ; "Percentage"
    ; "Currency"
    ; "ANSpace"
    ; "NonApplicable"
    ; "Date"
    ; "DateTime" ]
  in
  let ascIds = List.rev_map ~f:Id.create wrongway in
  ProcedPrefs.aHier_from_names ascIds w i g
;;

let hate_junk ?weight:(w = 5.0) (g : GrammarSpec.t) : ProcedPrefs.t =
  let j : Nts.t = Id "Junk", [] in
  let badTarget : Production.t = [Nonterminal j] in
  let all_pairs = GrammarSpec.as_kvp_list g in
  let f ((nid, prod) : NumberedId.t * Production.t) : int option =
    if Production.equal prod badTarget then Some nid.idx else None
  in
  let rl = List.filter_map ~f all_pairs in
  let _ = print_endline "Found Junk: " in
  let _ = print_endline (string_of_int (List.length rl)) in
  AntiPref (rl, w)
;;

let pref_parsimony ?weight:(w = 1.0) (g : GrammarSpec.t) : ProcedPrefs.t =
  ProcedPrefs.pref_parsimony w g
;;

let make_csv_preferences (g : GrammarSpec.t) : ProcedPrefs.t list option =
  let hier = parsing_hierarchy ~start:1.0 ~increment:2.0 g in
  let antijunk = hate_junk ~weight:5.0 g in
  let loveheaders = pref_headers ~weight:10.0 g in
  Some (antijunk :: loveheaders :: hier)
;;

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux (j - 1) []
;;

let rowsConstraint (g : GrammarSpec.t) : Constraint.t list =
  let iMax, jMax = 20, 30 in
  let name = Id.create "Rows" in
  let is : int list = 1 -- iMax in
  let js : int list = 1 -- jMax in
  let rec create_row j =
    let idxs : int list list = List.map ~f:(fun x -> [x; j]) is in
    ClunkyGrammar.GrammarSpec.cnstr_from_names_idxs name idxs g
  in
  let fullConstraints = List.map ~f:create_row js in
  List.concat fullConstraints
;;

let make_imply_constraints (g : GrammarSpec.t) : Constraint.t list =
  let implyconstraints : (string * string list) list =
    [ (*("URL", ["URL"; "InternetTerminal"; "URLChar"; "AlphaNumeric"; "URLWord"]);*)
      (*("Email", ["InternetTerminal"; "URLChar"; "AlphaNumeric"; "URLWord"]);*)
      (*("MultipleInt", ["MultipleInt"; "SingleInt"]);*)
      (*("SignedInt", ["MultipleInt"]);*)
      (*("Word", ["Word"; "Letter"]);*)
      (*("Letter", ["Letter"; "LLetter"; "ULetter"]);*)
      (*("AlphaNumeric", ["AlphaNumeric"; "SingleInt"; "Letter"]);*)
      (*("ANSpaceChar", ["SingleInt"; "Letter"]);*)
      (*("ANSpace", ["ANSpace";"ANSpaceChar"]);*) ]
  in
  List.map ~f:(fun nl -> GrammarSpec.imply_names nl g) implyconstraints
;;

let make_csv_constraints (g : GrammarSpec.t) : Constraint.t list =
  let constraints = rowsConstraint g in
  constraints
;;
