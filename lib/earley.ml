open MyStdLib
open ED2
open CG2
open NtsToNumber

type eChart = EarleyChart.t
type gramspec = GrammarSpec.t

exception EmptyChart of string
exception EmptyUnparsed of string
exception TerminalUnparsed

(*** Earley Parsing Helper Functions ***)
(* Predict *)
let create_items (item : EarleyItem.t) grammar startidx nullable ntsmap
    : EarleyItem.t list
  =
  let symbol_nts =
    match item.unparsed with
    | [] -> raise (EmptyUnparsed "in create_items")
    | Terminal _ :: _ -> raise TerminalUnparsed
    | Nonterminal nts :: _ -> nts
  in
  let old_prov = item.prov in
  let pairs = GrammarSpec.pairs_from_nts grammar ntsmap symbol_nts in
  let p2eis l (nid, prod) : EarleyItem.t list =
    let newitem =
      EarleyItem.create
        ~par:[]
        ~unpar:prod
        ~start:startidx
        ~head:nid
        ~prov:(ProvList.add nid old_prov)
    in
    (* puts in new rules *)
    newitem :: l
  in
  let basic_list = List.fold ~init:[] ~f:p2eis pairs in
  let prov = NullablesDict.lookup nullable symbol_nts in
  match ProvRecorder.extract_accepting_path prov with
  | None -> basic_list
  | Some prov ->
    let prov = List.map ~f:(fun i -> NumberedId.create i symbol_nts) prov in
    (match EarleyItem.advance_add_prov item prov with
    | None -> raise TerminalUnparsed
    | Some new_item -> new_item :: basic_list)
;;

(*let rec keep_advancing (last_item:EarleyItem.t) last_list =*)
(*match last_item.unparsed with*)
(*| [] -> last_list*)
(*| Terminal _ :: _ -> last_list*)
(*| Nonterminal last_symbol_nts :: _ ->*)
(*if NtsSet.mem last_symbol_nts nullable then*)
(*match EarleyItem.advance last_item with*)
(*| None -> raise (EmptyUnparsed "nullable symbol must exist")*)
(*| Some next_item -> keep_advancing next_item (last_item :: basic_list)*)
(*else*)
(*last_list *)
(*in keep_advancing item basic_list*)

(* Updates the current set in chart. returns stack of items to process and updated chart *)
let pred
    (item : EarleyItem.t)
    (grammar : gramspec)
    (chart : eChart)
    (idx : int)
    currentstack
    nullable
    ntsmap
    : EarleyItem.t list * eChart
  =
  (*let _ = print_endline ("\n --- Saw item" ^ (EarleyItem.show item)) in *)
  let currentdict =
    match EarleyChart.lookup chart idx with
    | None ->
      raise (EmptyChart ("Empty Chart in pred. Looked up " ^ string_of_int idx))
    | Some v -> v
  in
  let rec filter_duplicates ni l (d : EarleyDict.t) =
    match l with
    | [] -> ni, d
    | buffitem :: tl ->
      if EarleyDict.member_item currentdict buffitem
      then filter_duplicates ni tl d
      else (
        let snew = EarleyDict.insert_item d buffitem in
        let ninew = buffitem :: ni in
        filter_duplicates ninew tl snew)
  in
  let potential = create_items item grammar idx nullable ntsmap in
  (*let _ = print_endline ("Number of potential items: " ^ (string_of_int (List.length potential)))
         in *)
  let _ =
    match potential with
    | [a] ->
      (*let _ = print_endline ("The item \n" ^ (EarleyItem.show a)) in*) 0
    | _ -> 1
  in
  let newstack, newcurrent =
    filter_duplicates currentstack potential currentdict
  in
  let newchart = EarleyChart.insert chart idx newcurrent in
  newstack, newchart
;;

(* Scan *)
exception EmptyExpression
exception Impossible of string

let scanmatch (item : EarleyItem.t) : EarleyItem.t =
  match item.unparsed with
  | Terminal _ :: _ ->
    (match EarleyItem.advance item with
    | None -> raise (Impossible "In scanmatch, always advancable")
    | Some ni -> ni)
  | Nonterminal _ :: _ ->
    raise (Impossible "In scanmatch, next symbol should be terminal")
  | [] ->
    raise (Impossible "In scanmatch, there should always be a symbol to parse")
;;

(*Returns an update for the next buffset (index idx+1)*)
let scan
    (item : EarleyItem.t)
    (chart : eChart)
    (idx : int)
    (expr : Expr.t)
    (stack : EarleyItem.t list)
    : EarleyItem.t list * eChart
  =
  match EarleyChart.lookup chart (idx + 1) with
  | None -> stack, EarleyChart.remove_item idx item chart
  | Some nextdict ->
    let firstchar =
      match expr with
      | [] -> raise EmptyExpression
      | c :: _ -> c
    in
    (match item.unparsed with
    | Terminal charrange :: _ ->
      if CharRange.CRList.is_member firstchar charrange
      then (
        let nnd = EarleyDict.insert_item nextdict (scanmatch item) in
        let newchart = EarleyChart.insert chart (idx + 1) nnd in
        stack, newchart)
      else (
        let newchart = EarleyChart.remove_item idx item chart in
        stack, newchart)
    | _ ->
      raise
        (Impossible
           "In scan, there should always be a nonterminal symbol to parse"))
;;

(*| Nonterminal _ :: _ -> raise (NonterminalScan "Tried to scan nonterminal symbol")*)

(* Complete *)
exception TerminalComplete
exception ShortComplete of string

(*if there's a match, return the buff item that moves across the nonterminal*)
(*the first item is the completed item -- the second item is uncompleted.*)
let t_provU = ref 0.0
let t3 = ref 0.0
let reset (tx : float ref) : unit = tx := Sys.time ()

let upgrade (item : EarleyItem.t) (item2 : EarleyItem.t) : EarleyItem.t =
  match item2.unparsed with
  | [] -> raise (Impossible "always advancable")
  | Terminal _ :: _ -> raise (Impossible "always advancable")
  | nts :: tl ->
    reset t3;
    (*let pnew = ProvList.combine item.prov item2.prov in*)
    let pnew = ProvList.combine item2.prov item.prov in
    (*i think this is a better combine than the other way*)
    t_provU := !t_provU +. Sys.time () -. !t3;
    reset t3;
    EarleyItem.create
      ~par:(nts :: item2.parsed)
      ~unpar:tl
      ~start:item2.start
      ~head:item2.head
      ~prov:pnew
;;

let t_lookup = ref 0.0
let t_targetset = ref 0.0
let t_insert = ref 0.0
let t2 = ref 0.0
let reset_t2 () : unit = t2 := Sys.time ()

let incr_time (ti : float ref) : unit =
  ti := !ti +. (Sys.time () -. !t2);
  reset_t2 ()
;;

(*Returns stack of new items to process, and an updated version of currentset*)
let complete (item : EarleyItem.t) (chart : eChart) (idx : int) currentstack
    : EarleyItem.t list * eChart
  =
  let n = item.start in
  reset_t2 ();
  let currentdict =
    match EarleyChart.lookup chart idx with
    | None ->
      raise
        (EmptyChart
           ("Empty Chart in complete (currentdict). Looked up "
           ^ string_of_int idx))
    | Some v -> v
  in
  let prevdict =
    match EarleyChart.lookup chart n with
    | None ->
      raise
        (EmptyChart
           ("Empty Chart in complete (prevdict). Looked up "
           ^ string_of_int (idx - 1)))
    | Some v -> v
  in
  incr_time t_lookup;
  (*finished pulling 2 used charts*)
  let rec helper new_items updated_current remaining =
    match remaining with
    | [] -> new_items, updated_current
    | hd :: tl ->
      let newitem = upgrade item hd in
      helper
        (newitem :: new_items)
        (EarleyDict.insert_item updated_current newitem)
        tl
  in
  let target = Some (Symbol.Nonterminal item.head.id) in
  reset_t2 ();
  let targetSet =
    EarleyDict.lookup_default ~default:EarleySet.empty prevdict target
  in
  incr_time t_targetset;
  let newstack, newcurrentset =
    helper currentstack currentdict (EarleySet.as_list targetSet)
  in
  let _ = incr_time t_insert in
  let newchart = EarleyChart.insert chart idx newcurrentset in
  let _ = incr_time t_lookup (*in let _ = print_endline "Finished Complete"*) in
  newstack, newchart
;;

let time_pred = ref 0.0
let time_scan = ref 0.0
let time_complete = ref 0.0
let number_parses = ref 0
let t = ref 0.0

let print_specs () : unit =
  print_endline "Specs from Earley Parser";
  Printf.printf "Number of Parses: %i \n" !number_parses;
  Printf.printf "Pred time: %fs \n" !time_pred;
  Printf.printf "Scan time: %fs \n" !time_scan;
  Printf.printf "Complete time: %fs \n" !time_complete;
  Printf.printf "---Lookup time: %fs \n" !t_lookup;
  Printf.printf "---TargetSet time: %fs \n" !t_targetset;
  Printf.printf "---Insertion time: %fs \n" !t_insert;
  Printf.printf "---+++Prov Union time: %fs \n" !t_provU;
  ()
;;

let print_specs_experiment () : unit =
  print_endline (string_of_int !number_parses);
  print_endline ";";
  print_endline (string_of_float !time_pred);
  print_endline ";";
  print_endline (string_of_float !time_scan);
  print_endline ";";
  print_endline (string_of_float !time_complete);
  print_endline ";";
  print_endline (string_of_float !t_lookup);
  print_endline ";";
  print_endline (string_of_float !t_targetset);
  print_endline ";";
  print_endline (string_of_float !t_insert);
  print_endline ";";
  print_endline (string_of_float !t_provU);
  ()
;;

(*** Implementing Earley Algorithm ***)
(*inner_loop processes stack of items*)
let inner_loop
    (grammar : gramspec)
    (chart : eChart)
    (idx : int)
    (remaining_expression : Expr.t)
    nullable
    ntsmap
    : eChart
  =
  (*Aux will keep processing itmes in stack until there are none left.*)
  let rec aux (stack : EarleyItem.t list) (runningchart : eChart)
      : EarleyItem.t list * eChart
    =
    match stack with
    | [] -> [], runningchart
    | item :: stacktail ->
      let newstack, newchart =
        let _ = t := Sys.time () in
        match item.unparsed with
        | Nonterminal _ :: _ ->
          (*let _ = print_endline "Starting Pred" in *)
          let x =
            pred item grammar runningchart idx stacktail nullable ntsmap
          in
          let _ = time_pred := !time_pred +. (Sys.time () -. !t) in
          x
        | Terminal _ :: _ ->
          (*let _ = print_endline "Starting Scan" in *)
          let x = scan item runningchart idx remaining_expression stacktail in
          let _ = time_scan := !time_scan +. (Sys.time () -. !t) in
          x
        | [] ->
          (*let _ = print_endline "Starting Complete" in *)
          let x = complete item runningchart idx stacktail in
          let _ = time_complete := !time_complete +. (Sys.time () -. !t) in
          x
      in
      aux newstack newchart
  in
  match EarleyChart.lookup chart idx with
  | None ->
    raise
      (EmptyChart
         ("Empty Chart in complete (currentset). Looked up " ^ string_of_int idx))
  | Some dict ->
    let startstack = EarleyDict.all_values dict in
    let _, returned = aux startstack chart in
    returned
;;

(* iterates over every character in the expression. processes final EarleySet*)
let outer_loop
    (grammar : gramspec)
    (initial : eChart)
    (full_expression : Expr.t)
    nullable
    ntsmap
    : eChart
  =
  let rec aux runningchart idx remaining_expression =
    let newchart =
      inner_loop grammar runningchart idx remaining_expression nullable ntsmap
    in
    (*let _ = print_endline ("Chart after finishing index: " ^ (string_of_int idx)) in
          let _ = print_endline (EarleyChart.show newchart) in*)
    match remaining_expression with
    | [] ->
      inner_loop grammar runningchart idx [] nullable ntsmap
      (*processes last item*)
    | _ :: tl -> aux newchart (idx + 1) tl
  in
  aux initial 0 full_expression
;;

(* keys from : 0 -> expression length *)
let create_starting_chart (startdict : EarleyDict.t) (expr : Expr.t) : eChart =
  let n = List.length expr in
  let rec aux oldchart i max =
    if i > max
    then oldchart
    else (
      let chart =
        if i = 0
        then EarleyChart.insert oldchart 0 startdict
        else EarleyChart.insert oldchart i EarleyDict.empty
      in
      aux chart (i + 1) max)
  in
  aux EarleyChart.empty 0 n
;;

let create_starting_dict (grammar : gramspec) nts ntsToNumber : EarleyDict.t =
  let kvps = GrammarSpec.pairs_from_nts grammar ntsToNumber nts in
  let pairfold (currentdict : EarleyDict.t) (kvp : NumberedId.t * Production.t)
      : EarleyDict.t
    =
    let nid, prod = kvp in
    let newitem =
      EarleyItem.create
        ~par:[]
        ~unpar:prod
        ~start:0
        ~head:nid
        ~prov:(ProvList.add nid ProvList.empty)
    in
    EarleyDict.insert_item currentdict newitem
  in
  Stdlib.List.fold_left pairfold EarleyDict.empty kvps
;;

let process_last_set (lastset : EarleySet.t) (nts : Nts.t) : RuleSet.t option =
  (*let _ = print_endline "Final set: " in
      let _ = print_endline (EarleySet.show lastset) in*)
  let rec aux (l : EarleyItem.t list) =
    match l with
    | [] -> None
    | item :: tl ->
      let sameName = Nts.equal nts item.head.id in
      let startAt0 = item.start = 0 in
      if sameName && startAt0
      then Some (RuleSet.from_list item.prov)
      else aux tl
  in
  aux (EarleySet.as_list lastset)
;;

let process_last_dict (lastdict : EarleyDict.t) (nts : Nts.t) : RuleSet.t option
  =
  match EarleyDict.lookup lastdict None with
  | None -> None
  | Some es -> process_last_set es nts
;;

let parse (grammar : gramspec) (expression : Expr.t) (startNts : Nts.t)
    : RuleSet.t option
  =
  number_parses := !number_parses + 1;
  (*let _ = print_endline "Starting a parse" in *)
  let nullable = GrammarSpec.nullable_prov grammar in
  let ntsToNumber = GrammarSpec.nts_to_number_map grammar in
  let startdict = create_starting_dict grammar startNts ntsToNumber in
  let startchart = create_starting_chart startdict expression in
  (*
    let reachableSubgrammar = GrammarSpec.reachable grammar ntsToNumber startNts in 
    let endchart = outer_loop reachableSubgrammar startchart expression nullable ntsToNumber in
    *)
  let endchart =
    outer_loop grammar startchart expression nullable ntsToNumber
  in
  (*let _ = print_endline ("Final Chart: ") in
      let _ = print_endline (EarleyChart.show endchart) in*)
  let n = List.length expression in
  let lastset =
    match EarleyChart.lookup endchart n with
    | None -> EarleyDict.empty
    | Some v -> v
  in
  let res = process_last_dict lastset startNts in
  (*let _ = print_endline "Finished Processing Last Dictionary" in*)
  res
;;
