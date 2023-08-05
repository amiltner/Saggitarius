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
  let pairs = GrammarSpec.pairs_from_nts grammar ntsmap symbol_nts in
  let p2eis l (nid, prod) : EarleyItem.t list =
    let newitem =
      EarleyItem.create
        ~par:[]
        ~unpar:prod
        ~start:startidx
        ~head:nid
        ~prov:ProvList.empty
    in
    newitem :: l
  in
  let basic_list = List.fold ~init:[] ~f:p2eis pairs in
  if NtsSet.mem symbol_nts nullable
  then (
    match EarleyItem.advance item with
    | None -> raise TerminalUnparsed
    | Some new_item -> new_item :: basic_list)
  else basic_list
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
        (* let ninew = ni in *)
        filter_duplicates ninew tl snew)
  in
  let potential = create_items item grammar idx nullable ntsmap in
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
let upgrade (item : EarleyItem.t) (item2 : EarleyItem.t) : EarleyItem.t =
  match item2.unparsed with
  | [] -> raise (Impossible "always advancable")
  | Terminal _ :: _ -> raise (Impossible "always advancable")
  | nts :: tl ->
    EarleyItem.create
      ~par:[]
      ~unpar:tl
      ~start:item2.start
      ~head:item2.head
      ~prov:[]
;;

(*Returns stack of new items to process, and an updated version of currentset*)
let complete (item : EarleyItem.t) (chart : eChart) (idx : int) currentstack
    : EarleyItem.t list * eChart
  =
  let n = item.start in
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
  let targetSet =
    EarleyDict.lookup_default ~default:EarleySet.empty prevdict target
  in
  let newstack, newcurrentset =
    helper currentstack currentdict (EarleySet.as_list targetSet)
  in
  let newchart =
    EarleyChart.insert chart idx newcurrentset
    (*in let _ = print_endline "Finished Complete"*)
  in
  newstack, newchart
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
        match item.unparsed with
        | Nonterminal _ :: _ ->
          pred item grammar runningchart idx stacktail nullable ntsmap
        | Terminal _ :: _ ->
          scan item runningchart idx remaining_expression stacktail
        | [] -> complete item runningchart idx stacktail
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
    (*let _ = print_endline ("Chart after finishing index: " ^ (string_of_int idx)) in*)
    (*let _ = print_endline (EarleyChart.show newchart) in*)
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
        ~prov:ProvList.empty
    in
    EarleyDict.insert_item currentdict newitem
  in
  Stdlib.List.fold_left pairfold EarleyDict.empty kvps
;;

let process_last_set (lastset : EarleySet.t) (nts : Nts.t) : bool =
  let rec aux (l : EarleyItem.t list) =
    match l with
    | [] -> false
    | item :: tl ->
      let sameName = Nts.equal nts item.head.id in
      let startAt0 = item.start = 0 in
      if sameName && startAt0 then true else aux tl
  in
  aux (EarleySet.as_list lastset)
;;

let process_last_dict (lastdict : EarleyDict.t) (nts : Nts.t) : bool =
  match EarleyDict.lookup lastdict None with
  | None -> false
  | Some es -> process_last_set es nts
;;

let num_rec = ref 0

let print_specs () : unit =
  print_endline "Specs from Earley Recognizer";
  Printf.printf "Number of Recs: %i \n" !num_rec
;;

let print_specs_experiment () : unit = print_endline (string_of_int !num_rec)

let recognize (grammar : gramspec) (expression : Expr.t) (startNts : Nts.t)
    : bool
  =
  num_rec := !num_rec + 1;
  (*let _ = print_endline "Starting a parse" in *)
  let nullable = GrammarSpec.nullable_symbols grammar in
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
  (*let _ = print_endline "Finished Parse Attempt" in *)
  let n = List.length expression in
  let lastset =
    match EarleyChart.lookup endchart n with
    | None -> EarleyDict.empty
    | Some v -> v
  in
  process_last_dict lastset startNts
;;
