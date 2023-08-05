open Lib
open MyStdLib
open Core
module RS = RuleSelector.Make (Earley) (EarleyRecognizer)
module RSSH = SATHeavyRuleSelector.SATHeavy
module RSSL = SATLiteRuleSelector.SATLite

let convert_to_otherthing
    input_grammar
    printGrammar
    printRuntimeSpecs
    logProgress
    runExperiments
    satlite
    prosynth
    positives
    negatives
    pos_ndfo
    neg_ndfo
    size
    prod_size
    nt_size
    example_count
    random_min_examples
  =
  let select2, exp_printer =
    if satlite
    then RSSL.select2, RSSL.print_specs_experiment
    else if prosynth
    then RS.select2, RS.print_specs_experiment
    else RSSH.select2, RSSH.print_specs_experiment
  in
  Consts.log_progress := logProgress;
  let prebuilt_pos =
    List.map ~f:(fun fname -> SimpleFile.read_from_file ~fname) positives
    @ Option.value_map
        ~default:[]
        ~f:(fun f -> String.split ~on:'\n' (SimpleFile.read_from_file f))
        pos_ndfo
  in
  let prebuilt_neg =
    List.map ~f:(fun fname -> SimpleFile.read_from_file ~fname) negatives
    @ Option.value_map
        ~default:[]
        ~f:(fun f -> String.split ~on:'\n' (SimpleFile.read_from_file f))
        neg_ndfo
  in
  let _ = Consts.log "Starting Induction" in
  let problem =
    Parser.problem
      Lexer.token
      (Lexing.from_string (SimpleFile.read_from_file ~fname:input_grammar))
  in
  let problem =
    Problem.add_examples
      ~problem
      ~positives:prebuilt_pos
      ~negatives:prebuilt_neg
  in
  let problem = Problem.update_import_base problem input_grammar in
  let problem =
    fold_until_completion
      ~f:(fun (p, procd) ->
        match Problem.pop_import p with
        | None -> Right p
        | Some (import_name, p) ->
          if List.mem ~equal:String.equal procd import_name
          then Left (p, procd)
          else (
            let procd = import_name :: procd in
            let import =
              Parser.problem
                Lexer.token
                (Lexing.from_string
                   (SimpleFile.read_from_file ~fname:import_name))
            in
            let merge =
              Problem.update_import_base ~problem:import import_name
            in
            let p = Problem.merge_problem ~problem:p ~merge in
            Left (p, procd)))
      (problem, [])
  in
  let _ = Consts.log "Finished Parsing" in
  if size
  then print_endline (string_of_int (Problem.size problem))
  else if prod_size
  then print_endline (string_of_int (Problem.prod_size problem))
  else if nt_size
  then print_endline (string_of_int (Problem.nt_size problem))
  else if example_count
  then print_endline (string_of_int (Problem.example_count problem))
  else if random_min_examples then
    begin
      let problem = Problem.normalize_examples problem in
      let true_res =
        if Problem.contains_inftys problem
        then select2 ~problem
        else (
          let s2 i =
            let problem = Problem.replace_infty_with i problem in
            select2 ~problem
          in
          Some (do_until_completion ~init:1 ~step:1 s2))
      in
      let make_unified result =
        Option.map
          ~f:(fun (_,ntscrl,outs) ->
              (List.sort
                 ~compare:(pair_compare Lib.Nts.compare Lib.ConcretizedRegex.compare)
                ntscrl,outs))
          result
      in
      let true_res = make_unified true_res in
      let check_true_equal result =
        let result = make_unified result in
        let comparison = option_compare (pair_compare (compare_list (pair_compare Lib.Nts.compare Lib.ConcretizedRegex.compare)) (compare_list String.compare)) in
        0 = comparison result true_res
      in
      let positives = List.map ~f:(fun s -> (s,true)) problem.positives in
      let negatives = List.map ~f:(fun s -> (s,false)) problem.negatives in
      let empty_problem = Problem.clear_examples problem in
      let full_testbench = positives@negatives in
      fold_until_completion
        ~f:(fun (tb,remaining_tb) ->
            let problem = Problem.update_from_testbench empty_problem tb in
            let problem = Problem.normalize_examples problem in
            let res_o =
              if Problem.contains_inftys problem
              then select2 ~problem
              else (
                let s2 i =
                  let problem = Problem.replace_infty_with i problem in
                  select2 ~problem
                in
                Some (do_until_completion ~init:1 ~step:1 s2))
            in
            if check_true_equal res_o then
              Right (print_endline (string_of_int (List.length tb)))
            else
              let (extracted_ex,remaining_tb) = random_element remaining_tb in
              Left (extracted_ex::tb,remaining_tb))
        ([],full_testbench)
    end
  else (
    let res =
      if Problem.contains_inftys problem
      then select2 ~problem
      else (
        let s2 i =
          let problem = Problem.replace_infty_with i problem in
          select2 ~problem
        in
        Some (do_until_completion ~init:1 ~step:1 s2))
    in
    match res with
    | Some (_, nts,emits) ->
      if runExperiments
      then begin
        print_endline
          (Str.global_replace
             (Str.regexp ";")
             "SEMI"
             (Pp.pp_regex_nts_list nts));
        print_endline ";";
        exp_printer ()
      end
      else begin
        if printGrammar
        then begin
          print_endline "Found solution:";
          print_endline (Pp.pp_regex_nts_list nts);
          print_endline "Emits:";
          print_endline (string_of_list ident emits)
        end;
        if printRuntimeSpecs
        then begin
          let _ = RS.print_specs () in
          let _ = Earley.print_specs () in
          let _ = EarleyRecognizer.print_specs () in
          ()
        end
      end
    | None ->
      if runExperiments
      then begin
        print_endline "Found nothing";
        print_endline ";";
        exp_printer ()
      end
      else if printGrammar
      then begin
        print_endline "Found nothing";
        failwith "Found nothing"
      end;
      if printRuntimeSpecs
      then begin
        let _ = RS.print_specs () in
        let _ = Earley.print_specs () in
        let _ = EarleyRecognizer.print_specs () in
        ()
      end)
;;

open Command.Let_syntax

let param =
  Command.basic
    ~summary:"..."
    [%map_open
      let input_grammar = anon ("input_grammar" %: string)
      and no_grammar_output =
        flag
          "no-grammar-output"
          no_arg
          ~doc:"do not output the discovered grammar"
      and log_progress =
        flag "log-progress" no_arg ~doc:"output the progress log"
      and print_runtime_specs =
        flag "print-runtime-specs" no_arg ~doc:"output the runtime specs"
      and run_experiments =
        flag "run-experiments" no_arg ~doc:"output experient info"
      and satlite = flag "satlite" no_arg ~doc:"run with satlite"
      and prosynth = flag "prosynth" no_arg ~doc:"run with prosynth approach"
      and positive_examples =
        flag "pos" (listed string) ~doc:"path positive example path"
      and negative_examples =
        flag "neg" (listed string) ~doc:"path negative example path"
      and pos_ndfo =
        flag
          "pos-ndf"
          (optional string)
          ~doc:"path newline delimited positive example path"
      and neg_ndfo =
        flag
          "neg-ndf"
          (optional string)
          ~doc:"path newline delimited negative example path"
      and size = flag "size" no_arg ~doc:"get size"
      and prod_size = flag "prod-size" no_arg ~doc:"get prod size"
      and nt_size = flag "nt-size" no_arg ~doc:"get nt size"
      and example_count = flag "example-count" no_arg ~doc:"get example count"
      and random_min_examples = flag "random-min-examples" no_arg ~doc:"get example count when randomizing examples iteratively increasing" in
      fun () ->
        convert_to_otherthing
          input_grammar
          (not no_grammar_output)
          print_runtime_specs
          log_progress
          run_experiments
          satlite
          prosynth
          positive_examples
          negative_examples
          pos_ndfo
          neg_ndfo
          size
          prod_size
          nt_size
          example_count
          random_min_examples]
;;

let () = Command.run param
