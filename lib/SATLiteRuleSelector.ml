open MyStdLib
open ClunkyGrammar
open EarleyFullProvs

exception Impossible

module SATLite : RuleSelector.Selector = struct
  let num_steps = ref 0
  let time_solving = ref 0.
  let time_parsing = ref 0.
  let time_subgrammaring = ref 0.
  let time_dding = ref 0.
  let time_trimming = ref 0.
  let print_specs () = ()

  let print_specs_experiment () =
    print_endline (string_of_int !num_steps);
    print_endline ";";
    print_endline (string_of_float !time_solving);
    print_endline ";";
    print_endline (string_of_float !time_parsing);
    print_endline ";";
    print_endline (string_of_float !time_subgrammaring);
    print_endline ";";
    print_endline (string_of_float !time_dding);
    print_endline ";";
    print_endline (string_of_float !time_trimming);
    print_endline ";";
    EarleyFullProvs.print_specs_experiment ();
    print_endline ";";
    print_endline (string_of_int 0);
    (*num recognition calls*)
    ()
  ;;

  (*Returns provenances that we were not supposed to get.*)
  let false_positives
      (grammar : GrammarSpec.t)
      (used : NumberedId.t list)
      (unused : NumberedId.t list)
      (startNts : Nts.t)
      ~negativePRs
      : RuleSet.t list option
    =
    let rs = RuleSet.from_list used in
    let fps =
      List.filter_map ~f:(fun x -> SATLiteParser.parse x rs) negativePRs
    in
    match fps with
    | [] -> None
    | l -> Some l
  ;;

  (*Returns provenances whose rules we must include at least 1 of*)
  let false_negatives
      (grammar : GrammarSpec.t)
      (used : NumberedId.t list)
      ~positivePRs
      : RuleSet.t list option
    =
    let rs = RuleSet.from_list used in
    let fns =
      List.filter
        ~f:(fun pr -> not (SATLiteRecognizer.check_membership pr rs))
        positivePRs
    in
    let allRules = RuleSet.from_list (GrammarSpec.key_list grammar) in
    let unused = RuleSet.diff allRules rs in
    match fns with
    | [] -> None
    | l ->
      let debug falseNegPR =
        let fails (excluded : RuleSet.t) : bool =
          (*true if cannot derive*)
          let sg = RuleSet.diff allRules excluded in
          let b = SATLiteRecognizer.check_membership falseNegPR sg in
          not b
        in
        DeltaDebugger.deltaDebugger (RuleSet.as_list unused) fails
      in
      Some (List.map ~f:debug fns)
  ;;

  let select
      (il : IL.t)
      (positives : Expr.t list)
      (negatives : Expr.t list)
      (startNts : Nts.t)
      : GrammarSpec.t option
    =
    let i, c, sels, prefs = il in
    let _ = Consts.log (IL.show il) in
    let grammar = IL.to_grammar il in
    let positivePRs : ProvRecorder.t list =
      List.map ~f:(fun x -> EarleyFullProvs.parse grammar x startNts) positives
    in
    let negativePRs : ProvRecorder.t list =
      List.map ~f:(fun x -> EarleyFullProvs.parse grammar x startNts) negatives
    in
    let rec aux sat : RuleSet.t option =
      (* SAT Solving *)
      match SMTSolver.solve sat prefs with
      | None -> None
      | Some m ->
        let model = m in
        (* Parsing *)
        let used = SMTSolver.get_positives grammar model in
        let unused = SMTSolver.get_negatives grammar model in
        let fps = false_positives grammar used unused startNts ~negativePRs in
        let fns = false_negatives grammar used ~positivePRs in
        (match fps, fns with
        | None, None -> Some (RuleSet.from_list used)
        | Some fplist, None ->
          let newSat =
            List.fold_left fplist ~init:sat ~f:(fun s p ->
                SMTSolver.add_why_prov p s)
          in
          aux newSat
        | None, Some fnlist ->
          let newSat =
            List.fold_left fnlist ~init:sat ~f:(fun s p ->
                SMTSolver.add_whynot_prov p s)
          in
          aux newSat
        | Some fplist, Some fnlist ->
          let newSat1 =
            List.fold_left fplist ~init:sat ~f:(fun s p ->
                SMTSolver.add_why_prov p s)
          in
          let newSat2 =
            List.fold_left fnlist ~init:newSat1 ~f:(fun s p ->
                SMTSolver.add_whynot_prov p s)
          in
          aux newSat2)
      (*First Solution. Will cut it down to a minimal working solution*)
    in
    let constraint_reqs = SMTSolver.init_from_IL il in
    let solution = aux constraint_reqs in
    match solution with
    | None -> None
    | Some prov ->
      let parses =
        List.filter_map ~f:(fun x -> SATLiteParser.parse x prov) positivePRs
      in
      let combined =
        List.fold_left
          parses
          ~init:RuleSet.empty
          ~f:(fun (x : RuleSet.t) (y : RuleSet.t) -> RuleSet.union x y)
      in
      (*in let result = trim_result solution positives negatives grammar startNts constraint_reqs in*)
      let g =
        ClunkyGrammar.GrammarSpec.subgrammar_prov (IL.to_grammar il) combined
      in
      Some g
  ;;

  let select2 ~problem:(p : Problem.t)
      : (GrammarSpec.t * (Nts.t * ConcretizedRegex.t) list * string list) option
    =
    let startNts = p.start, [] in
    let positives = List.map ~f:String.to_list p.positives in
    let negatives = List.map ~f:String.to_list p.negatives in
    let (il,funs) = Problem.to_il p in
    let i, c, sels, prefs = il in
    let _ = Consts.log (IL.show il) in
    let grammar = IL.to_grammar il in
    let positivePRs : ProvRecorder.t list =
      Consts.time
        (fun _ ->
          List.map
            ~f:(fun x -> EarleyFullProvs.parse grammar x startNts)
            positives)
        time_parsing
    in
    let negativePRs : ProvRecorder.t list =
      Consts.time
        (fun _ ->
          List.map
            ~f:(fun x -> EarleyFullProvs.parse grammar x startNts)
            negatives)
        time_parsing
    in
    let rec aux sat : RuleSet.t option =
      (* SAT Solving *)
      num_steps := !num_steps + 1;
      let s = Consts.time (fun _ -> SMTSolver.solve sat prefs) time_solving in
      match s with
      | None -> None
      | Some m ->
        let model = m in
        (* Parsing *)
        let used = SMTSolver.get_positives grammar model in
        let unused = SMTSolver.get_negatives grammar model in
        let fps = false_positives grammar used unused startNts ~negativePRs in
        let fns = false_negatives grammar used ~positivePRs in
        (match fps, fns with
        | None, None -> Some (RuleSet.from_list used)
        | Some fplist, None ->
          let newSat =
            List.fold_left fplist ~init:sat ~f:(fun s p ->
                SMTSolver.add_why_prov p s)
          in
          aux newSat
        | None, Some fnlist ->
          let newSat =
            List.fold_left fnlist ~init:sat ~f:(fun s p ->
                SMTSolver.add_whynot_prov p s)
          in
          aux newSat
        | Some fplist, Some fnlist ->
          let newSat1 =
            List.fold_left fplist ~init:sat ~f:(fun s p ->
                SMTSolver.add_why_prov p s)
          in
          let newSat2 =
            List.fold_left fnlist ~init:newSat1 ~f:(fun s p ->
                SMTSolver.add_whynot_prov p s)
          in
          aux newSat2)
      (*First Solution. Will cut it down to a minimal working solution*)
    in
    let constraint_reqs = SMTSolver.init_from_IL il in
    let solution = aux constraint_reqs in
    match solution with
    | None -> None
    | Some prov ->
      (*let parses = List.filter_map 
        ~f:(fun x -> SATLiteParser.parse x prov) positivePRs in
    let combined = List.fold_left parses ~init:(RuleSet.empty) ~f:(fun (x:RuleSet.t) (y:RuleSet.t) -> RuleSet.union x y) in*)
      (*in let result = trim_result solution positives negatives grammar startNts constraint_reqs in*)
      let g =
        ClunkyGrammar.GrammarSpec.subgrammar_prov (IL.to_grammar il) prov
      in
      let g =
        Consts.time
          (fun _ -> GrammarSpec.reachable_plain g startNts)
          time_trimming
      in
      let g_ints =
        List.map ~f:(fun k -> k.idx) (ClunkyGrammar.GrammarSpec.key_list g)
      in
      let variousstuff = List.filter_map ~f:(IL.IntToNtsRegex.lookup sels) g_ints in
      let ids = List.map ~f:(fun x -> x.idx) (RuleSet.as_list prov) in
      Some (g, variousstuff, funs ids)
  ;;
end
