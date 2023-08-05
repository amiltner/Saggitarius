open MyStdLib
open ClunkyGrammar
open EarleyFullProvs
open EarleyFullProvRecognizer
open Constraint.Z

exception Impossible

module SATHeavy : RuleSelector.Selector = struct
  (**** SAT-Heavy implementation of full provenances ****)

  let print_specs () = ()
  let time_solving = ref 0.0
  let time_parsing = ref 0.0
  let time_trimming = ref 0.0

  let print_specs_experiment () =
    print_endline (string_of_int 1);
    (*num rounds*)
    print_endline ";";
    print_endline (string_of_float !time_solving);
    print_endline ";";
    print_endline (string_of_float !time_parsing);
    print_endline ";";
    print_endline (string_of_float 0.0);
    (*subgrammaring*)
    print_endline ";";
    print_endline (string_of_float 0.0);
    (*deltadebugging*)
    print_endline ";";
    print_endline (string_of_float !time_trimming);
    (*trimming*)
    print_endline ";";
    EarleyFullProvs.print_specs_experiment ();
    print_endline ";";
    print_endline (string_of_int 0);
    (*num recognition calls*)
    ()
  ;;

  (*Returns SAT term specifying that we need one of the following provenances
   * for each of the positive examples.*)
  let need_positives (grammar : GrammarSpec.t) (startNts : Nts.t) ~positives
      : [> Constraint.Z.zbool ] Constraint.Z.term
    =
    let posPrs : ProvRecorder.t list =
      Consts.time
        (fun _ ->
          List.map
            ~f:(fun x -> EarleyFullProvs.parse grammar x startNts)
            positives)
        time_parsing
    in
    let termList = List.map ~f:EarleyFullProvRecognizer.need_one_SAT posPrs in
    T.and_ termList
  ;;

  (*Returns provenances whose rules we must include at least 1 of*)
  let no_negatives (grammar : GrammarSpec.t) (startNts : Nts.t) ~negatives
      : [> Constraint.Z.zbool ] Constraint.Z.term
    =
    let negPrs : ProvRecorder.t list =
      Consts.time
        (fun _ ->
          List.map
            ~f:(fun x -> EarleyFullProvs.parse grammar x startNts)
            negatives)
        time_parsing
    in
    let termList = List.map ~f:EarleyFullProvRecognizer.cant_have_SAT negPrs in
    T.and_ termList
  ;;

  let select
      (il : IL.t)
      (positives : Expr.t list)
      (negatives : Expr.t list)
      (startNts : Nts.t)
      : GrammarSpec.t option
    =
    let _ = Consts.log (IL.show il) in
    let grammar = IL.to_grammar il in
    (* Code specifict to SAT-Heavy implementation of full provenances *)
    let positiveTerms = need_positives grammar startNts ~positives in
    let negativeTerms = no_negatives grammar startNts ~negatives in
    let constraints = SMTSolver.init_from_IL il in
    let full_constraints = T.and_ [constraints; positiveTerms; negativeTerms] in
    let _, _, _, prefs = il in
    let model =
      Consts.time (fun _ -> SMTSolver.solve full_constraints prefs) time_solving
    in
    match model with
    | None -> None
    | Some m ->
      let used = SMTSolver.get_positives grammar m in
      let prov = RuleSet.from_list used in
      let sg = GrammarSpec.subgrammar_prov grammar prov in
      Some sg
  ;;

  let select2 ~problem:(p : Problem.t)
    : (GrammarSpec.t * (Nts.t * ConcretizedRegex.t) list * (string list)) option
    =
    let startNts = p.start, [] in
    let positives = List.map ~f:String.to_list p.positives in
    let negatives = List.map ~f:String.to_list p.negatives in
    let (il,funs) = Problem.to_il p (*p.cform p.start p.spaces*) in
    let i, c, sels, prefs = il in
    let _ = Consts.log (IL.show il) in
    let grammar = IL.to_grammar il in
    (* Code specifict to SAT-Heavy implementation of full provenances *)
    let positiveTerms = need_positives grammar startNts ~positives in
    let negativeTerms = no_negatives grammar startNts ~negatives in
    let constraints = SMTSolver.init_from_IL il in
    let full_constraints = T.and_ [constraints; positiveTerms; negativeTerms] in
    let model =
      Consts.time (fun _ -> SMTSolver.solve full_constraints prefs) time_solving
    in
    match model with
    | None -> None
    | Some m ->
      let used = SMTSolver.get_positives grammar m in
      let prov = RuleSet.from_list used in
      let g = GrammarSpec.subgrammar_prov grammar prov in
      let g =
        Consts.time
          (fun _ -> GrammarSpec.reachable_plain g startNts)
          time_trimming
      in
      (*let parses = List.filter_map 
        ~f:(fun x -> P.parse subgrammar x startNts) positives in
      let combined = List.fold_left parses ~init:(RuleSet.empty) ~f:(fun (x:RuleSet.t) (y:RuleSet.t) -> RuleSet.union x y) in
      (*in let result = trim_result solution positives negatives grammar startNts constraint_reqs in*)
      let _ = update_time time_trimming*)
      let g_ints =
        List.map ~f:(fun k -> k.idx) (ClunkyGrammar.GrammarSpec.key_list g)
      in
      let various = List.filter_map ~f:(IL.IntToNtsRegex.lookup sels) g_ints in
      let ids = List.map ~f:(fun x -> x.idx) (RuleSet.as_list prov) in
      Some (g, various, funs ids)
  ;;
end
