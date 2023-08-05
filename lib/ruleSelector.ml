open MyStdLib
open ClunkyGrammar

module type Selector = sig
  val select
    :  IL.t
    -> Expr.t list
    -> Expr.t list
    -> Nts.t
    -> GrammarSpec.t option

  val select2
    :  problem:Problem.t
    -> (GrammarSpec.t * (Nts.t * ConcretizedRegex.t) list * string list) option

  val print_specs : unit -> unit
  val print_specs_experiment : unit -> unit
end

module Make (P : ParserSig.Parser) (R : Recognizer.Recognizer) : Selector =
struct
  exception Impossible

  (* A bunch of timing parameters *)
  let num_steps = ref 0
  let t = ref 0.0
  let time_solving = ref 0.0
  let time_parsing = ref 0.0
  let time_subgrammaring = ref 0.0
  let time_dding = ref 0.0
  let time_trimming = ref 0.0
  let reset_time () = t := Sys.time ()

  let update_time t1 =
    t1 := !t1 +. (Sys.time () -. !t);
    reset_time ()
  ;;

  (*Returns provenances that we were not supposed to get.*)
  let false_positives
      (grammar : GrammarSpec.t)
      (used : NumberedId.t list)
      (unused : NumberedId.t list)
      (startNts : Nts.t)
      ~negatives
      : RuleSet.t list option
    =
    reset_time ();
    let subgrammar = GrammarSpec.subgrammar grammar used in
    update_time time_subgrammaring;
    let fps =
      List.filter_map ~f:(fun x -> P.parse subgrammar x startNts) negatives
    in
    update_time time_parsing;
    match fps with
    | [] -> None
    | l -> Some l
  ;;

  (*Returns provenances whose rules we must include at least 1 of*)
  let false_negatives
      (grammar : GrammarSpec.t)
      (used : NumberedId.t list)
      (unused : NumberedId.t list)
      (startNts : Nts.t)
      ~positives
      : RuleSet.t list option
    =
    reset_time ();
    let subgrammar = GrammarSpec.subgrammar grammar used in
    time_subgrammaring := !time_subgrammaring +. (Sys.time () -. !t);
    let fns =
      List.filter
        ~f:(fun x -> not (R.recognize subgrammar x startNts))
        positives
    in
    match fns with
    | [] -> None
    | l ->
      let debug falseNeg =
        let fails (excluded : RuleSet.t) : bool =
          (*true if cannot derive*)
          reset_time ();
          let sg = GrammarSpec.complement_prov grammar excluded in
          update_time time_subgrammaring;
          let b = R.recognize sg falseNeg startNts in
          let _ = update_time time_parsing in
          not b
        in
        reset_time ();
        let x = DeltaDebugger.deltaDebugger unused fails in
        update_time time_dding;
        x
      in
      Some (List.map ~f:debug fns)
  ;;

  let trim_result_short solution positives _ grammar startNts _ =
    match solution with
    | None -> None
    | Some soln ->
      (match positives with
      | [] -> raise Impossible
      | hd :: tl ->
        let subgrammar =
          GrammarSpec.subgrammar grammar (RuleSet.as_list soln)
        in
        P.parse subgrammar hd startNts)
  ;;

  let trim_result solution positives negatives grammar startNts constraint_reqs =
    DeltaDebugger.printflag := false;
    match solution with
    | None -> None
    | Some soln ->
      let derives (example : Expr.t) (prov : RuleSet.t) : bool =
        let sg = GrammarSpec.subgrammar_prov grammar prov in
        R.recognize sg example startNts
      in
      let worksForPositives tp =
        List.for_all positives (fun x -> derives x tp)
      in
      let worksForSAT tp =
        let unused =
          RuleSet.diff (RuleSet.from_list (GrammarSpec.key_list grammar)) tp
        in
        SMTSolver.test_prov constraint_reqs tp unused
      in
      let fullTest tp =
        worksForPositives tp && worksForSAT tp
        (* (worksForPositives tp) *)
      in
      Some (DeltaDebugger.deltaDebugger (RuleSet.as_list soln) fullTest)
  ;;

  let print_specs () : unit =
    print_endline "Specs from full algorithm";
    Printf.printf "Iterations (SATs Solved): %i \n" !num_steps;
    Printf.printf "Solving time: %fs \n" !time_solving;
    Printf.printf "Parsing time: %fs \n" !time_parsing;
    Printf.printf "Subgrammaring time: %fs \n" !time_subgrammaring;
    Printf.printf "DeltaDebugging time: %fs \n" !time_dding;
    Printf.printf "Trimming time: %fs \n" !time_trimming;
    ()
  ;;

  let print_specs_experiment () : unit =
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
    P.print_specs_experiment ();
    print_endline ";";
    R.print_specs_experiment ();
    ()
  ;;

  let select
      (il : IL.t)
      (positives : Expr.t list)
      (negatives : Expr.t list)
      (startNts : Nts.t)
      : GrammarSpec.t option
    =
    let i, c, d, prefs = il in
    let _ = Consts.log (IL.show il) in
    num_steps := 0;
    time_solving := 0.0;
    time_parsing := 0.0;
    time_trimming := 0.0;
    time_subgrammaring := 0.0;
    time_dding := 0.0;
    let grammar = IL.to_grammar il in
    (* let sat = SMTSolver.start grammar in *)
    let rec aux sat : RuleSet.t option =
      num_steps := !num_steps + 1;
      reset_time ();
      (* SAT Solving *)
      match SMTSolver.solve sat prefs with
      | None -> None
      | Some m ->
        let model = m in
        update_time time_solving;
        reset_time ();
        (* Parsing *)
        let used = SMTSolver.get_positives grammar model in
        let unused = SMTSolver.get_negatives grammar model in
        let fps = false_positives grammar used unused startNts ~negatives in
        let fns = false_negatives grammar used unused startNts ~positives in
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
    reset_time ();
    let solution = aux constraint_reqs in
    match solution with
    | None -> None
    | Some prov ->
      let subgrammar = GrammarSpec.subgrammar_prov grammar prov in
      let parses =
        List.filter_map ~f:(fun x -> P.parse subgrammar x startNts) positives
      in
      let combined =
        List.fold_left
          parses
          ~init:RuleSet.empty
          ~f:(fun (x : RuleSet.t) (y : RuleSet.t) -> RuleSet.union x y)
      in
      (*in let result = trim_result solution positives negatives grammar startNts constraint_reqs in*)
      let _ = update_time time_trimming in
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
    let _ = Consts.log "Finished Converting to IL" in
    let _ = Consts.log "Finished Converting from IL" in
    let (i, c, sels, prefs) = il in
    let _ = Consts.log (IL.show il) in
    num_steps := 0;
    time_solving := 0.0;
    time_parsing := 0.0;
    time_trimming := 0.0;
    time_subgrammaring := 0.0;
    time_dding := 0.0;
    let grammar = IL.to_grammar il in
    (* let sat = SMTSolver.start grammar in *)
    let rec aux sat : RuleSet.t option =
      num_steps := !num_steps + 1;
      reset_time ();
      (* SAT Solving *)
      match SMTSolver.solve sat prefs with
      | None -> None
      | Some m ->
        let model = m in
        update_time time_solving;
        reset_time ();
        (* Parsing *)
        let used = SMTSolver.get_positives grammar model in
        let unused = SMTSolver.get_negatives grammar model in
        let fps = false_positives grammar used unused startNts ~negatives in
        let fns = false_negatives grammar used unused startNts ~positives in
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
    reset_time ();
    let solution = aux constraint_reqs in
    match solution with
    | None -> None
    | Some prov ->
      (*let subgrammar = GrammarSpec.subgrammar_prov grammar prov in*)

      (*let parses = List.filter_map 
          ~f:(fun x -> P.parse subgrammar x startNts) positives in
        let combined = List.fold_left parses ~init:(RuleSet.empty) ~f:(fun (x:RuleSet.t) (y:RuleSet.t) -> RuleSet.union x y) in*)
      (*in let result = trim_result solution positives negatives grammar startNts constraint_reqs in*)
      let g =
        ClunkyGrammar.GrammarSpec.subgrammar_prov (IL.to_grammar il) prov
      in
      let g = GrammarSpec.reachable_plain g startNts in
      let _ = update_time time_trimming in
      let g_ints =
        List.map ~f:(fun k -> k.idx) (ClunkyGrammar.GrammarSpec.key_list g)
      in
      let ntsregex = List.filter_map ~f:(IL.IntToNtsRegex.lookup sels) g_ints in
      let ids = List.map ~f:(fun x -> x.idx) (RuleSet.as_list prov) in
      Some (g, ntsregex, funs ids)
  ;;
end
