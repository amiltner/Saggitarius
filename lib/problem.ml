open MyStdLib

type t =
  { imports : string list
  ; prods : MProd.t list
  ; constraints : BoolExp.t list
  ; start : Id.t
  ; positives : string list
  ; negatives : string list
  ; prefs : Pholder.t list
  ; global_symbolics : RangeBinding.t list
  ; emits : Eholder.t list
  }
[@@deriving eq, hash, ord, show]

let replace_infty_with (i : int) (p : t) : t =
  { imports = p.imports
  ; prods = List.map ~f:(MProd.replace_infty_with i) p.prods
  ; constraints = List.map ~f:(BoolExp.replace_infty_with i) p.constraints
  ; start = p.start
  ; positives = p.positives
  ; negatives = p.negatives
  ; prefs = List.map ~f:(Pholder.replace_infty_with i) p.prefs
  ; global_symbolics = List.map ~f:(RangeBinding.replace_infty_with i) p.global_symbolics
  ; emits = List.map ~f:(Eholder.replace_infty_with i) p.emits
  }
;;

let size (p : t) : int =
  assert (List.is_empty p.imports);
  let pds = List.map ~f:MProd.size p.prods in
  let css = List.map ~f:BoolExp.size p.constraints in
  let ss = 1 in
  let pss = List.map ~f:Pholder.size p.prefs in
  sum_int_list pds + sum_int_list css + ss + sum_int_list pss
;;

let nt_size (p : t) : int =
  assert (List.is_empty p.imports);
  List.length p.prods
;;

let prod_size (p : t) : int =
  assert (List.is_empty p.imports);
  let ss = List.map ~f:MProd.prod_size p.prods in
  sum_int_list ss
;;

let clear_examples (p:t) : t =
  { p with
    positives = [] ;
    negatives = [] ; }

let update_from_testbench (p:t) (testbench:(string * bool) list) : t =
  let (positives,negatives) =
    split_by_condition
      testbench
      snd
  in
  let positives = List.map ~f:fst positives in
  let negatives = List.map ~f:fst negatives in
  { p with
    positives ;
    negatives ; }

let normalize_examples (p:t)
  : t =
  { p with
    positives = List.sort ~compare:String.compare p.positives ;
    negatives = List.sort ~compare:String.compare p.negatives ;
  }

let example_count (p : t) : int =
  assert (List.is_empty p.imports);
  (List.length p.positives) + (List.length p.negatives)
;;

let contains_inftys (p : t) : bool =
  let rp = replace_infty_with 10 p in
  equal p rp
;;

let make
    ~(imports : string list)
    ~(prods : MProd.t list)
    ~(constraints : BoolExp.t list)
    ~(start : Id.t)
    ~(positives : string list)
    ~(negatives : string list)
    ~(prefs : Pholder.t list)
    ~(global_symbolics : RangeBinding.t list)
    ~(emits:Eholder.t list)
    : t
  =
  { imports; prods=(MProd.make ~head:(Id.create "TrueStart",[]) ~spaces:[Space.Singleton (false,Exp.True,None,Regex.Nonterminal (start,[]))])::prods; constraints; start=(Id.create "TrueStart"); positives; negatives; prefs; global_symbolics; emits; }
;;

let add_examples
    ~(problem : t)
    ~(positives : string list)
    ~(negatives : string list)
    : t
  =
  { problem with
    positives = problem.positives @ positives
  ; negatives = problem.negatives @ negatives
  }
;;

let add_prefs ~(problem : t) ~(prefs : Pholder.t list) : t =
  { problem with prefs = problem.prefs @ prefs }
;;

let add_prods ~(problem : t) ~(prods : MProd.t list) : t =
  { problem with prods = prods @ problem.prods }
;;

let add_constraints ~(problem : t) ~(constraints : BoolExp.t list) : t =
  { problem with constraints = constraints @ problem.constraints }
;;

let pop_import ~(problem : t) : (string * t) option =
  match problem.imports with
  | [] -> None
  | i :: imports -> Some (i, { problem with imports })
;;

let merge_problem ~(problem : t) ~(merge : t) : t =
  { problem with
    imports = merge.imports @ problem.imports
  ; prods = merge.prods @ problem.prods
  ; constraints = merge.constraints @ problem.constraints
  ; prefs = merge.prefs @ problem.prefs
  }
;;

let update_import_base ~(problem : t) (fname : string) : t =
  let dir = Filename.dirname fname in
  { problem with imports = List.map ~f:(( ^ ) (dir ^ "/")) problem.imports }
;;

module S = SetOf (Nts)

let mprods_to_il (mps : MProd.t list) : ConversionAcc.t =
  List.fold ~f:MProd.to_il ~init:ConversionAcc.empty mps
;;

let constraints_to_il
    (acc : ConversionAcc.t)
    (pm : (Nts.t * int list) list)
    (bs : BoolExp.t list)
    : Constraint.t list
  =
  List.map ~f:(BoolExp.to_constraint [] acc.index_map pm) bs
;;

let to_il (p : t) : IL.t * (int list -> string list) =
  let global_symbolics =
    List.map
      ~f:(fun (i,r) ->
          let (lr,hr) = Exp.eval_range [] r in
          (i,lr,hr))
      p.global_symbolics
  in
  let prods = List.map ~f:(MProd.update_ranges_from_symbolics global_symbolics) p.prods in
  let (prods,updateds) =
    List.unzip
      (List.map
         ~f:(fun mp ->
             MProd.update_args_if_needed_from_symbolics
               global_symbolics
               mp)
         prods)
  in
  let updateds = List.concat updateds in
  let prods =
    List.map
      ~f:(MProd.update_callsites_if_needed_from_symbolics updateds)
      prods
  in
  let acc = mprods_to_il prods in
  let pm = IndexedProduction.list_to_pm acc.ips in
  let constraints =
    List.map
      ~f:(fun bexp ->
          List.fold
            ~f:(fun bexp (i,lr,hr) ->
                Exp.bool_exp_replace_range_with
                  i
                  lr
                  hr
                  bexp)
            ~init:bexp
            global_symbolics)
      p.constraints
  in
  let prefs =
    List.map
      ~f:(fun bexp ->
          List.fold
            ~f:(fun bexp (i,lr,hr) ->
                Pholder.replace_range_with
                  i
                  lr
                  hr
                  bexp)
            ~init:bexp
            global_symbolics)
      p.prefs
  in
  let constraints = constraints_to_il acc pm constraints in
  let prefs =
    List.concat_map
      ~f:(fun p -> ProcedPrefs.from_pholder [] acc.index_map pm p)
      prefs
  in
  let emits =
      Eholder.full_emit_funs [] acc.index_map pm p.emits
  in
  (( acc.ips
  , constraints
  , IL.IntToNtsRegex.from_kvp_list acc.concretized_map
   , prefs ),emits)
;;
