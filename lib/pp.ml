open MyStdLib

let pp_nts ((i, is) : Nts.t) : string =
  let (Id id_string) = i in
  let is_strings = List.map ~f:(fun i -> "_" ^ string_of_int i) is in
  id_string ^ String.concat is_strings
;;

let rec pp_regex (r : ConcretizedRegex.t) : string =
  match r with
  | CharSet cr -> CharRange.CRList.show cr
  | Disjunct ds ->
    let ds_strings = List.map ~f:pp_regex ds in
    "(" ^ String.concat ~sep:" | " ds_strings ^ ")"
  | Conjunct cs ->
    let cs_strings = List.map ~f:pp_regex cs in
    String.concat ~sep:" " cs_strings
  | Star r -> "(" ^ pp_regex r ^ ")*"
  | Nonterminal nts -> pp_nts nts
;;

let pp_regex_nts_list (r : (Nts.t * ConcretizedRegex.t) list) : string =
  let group = group_by_keys ~is_eq:Nts.equal r in
  let grouped_strings =
    List.map
      ~f:(fun (nts, rs) ->
        let nts_string = pp_nts nts in
        let r_strings = List.map ~f:pp_regex rs in
        let rs_string = String.concat ~sep:"|" r_strings in
        nts_string ^ " -> " ^ rs_string)
      group
  in
  String.concat ~sep:"\n" grouped_strings
;;
