open MyStdLib

type disjunct_prov = Disjunct of (int * conjunct_prov list)

and conjunct_prov = Conjunct of disjunct_prov list
[@@deriving eq, hash, ord, show]

type t =
  | Or of t list
  | And of t list
  | Rule of int
[@@deriving eq, hash, ord, show]

let compress_multi_or (ps : t list) : t =
  let rec flatten (p : t) : t list =
    match p with
    | Or ps -> List.concat_map ~f:flatten ps
    | _ -> [p]
  in
  let flattened = List.concat_map ~f:flatten ps in
  let flattened = List.dedup_and_sort ~compare flattened in
  match flattened with
  | [h] -> h
  | _ -> Or flattened
;;

let multi_or (ps : t list) : t =
  if !Consts.minify then compress_multi_or ps else Or ps
;;

let zero : t = Or []
let one : t = And []
let or_ (p1 : t) (p2 : t) : t = multi_or [p1; p2]

let compress_multi_and (ps : t list) : t =
  let rec flatten (p : t) : t list =
    match p with
    | And ps -> List.concat_map ~f:flatten ps
    | _ -> [p]
  in
  let flattened = List.concat_map ~f:flatten ps in
  let flattened = List.dedup_and_sort ~compare flattened in
  match flattened with
  | [h] -> h
  | _ -> And flattened
;;

let multi_and (ps : t list) : t =
  if !Consts.minify then compress_multi_and ps else And ps
;;

let and_ (p1 : t) (p2 : t) : t = multi_and [p1; p2]
let rec rule (i : int) : t = Rule i
(*let rec simple_useless_rule
      (p:t)
    : bool =
    begin match p with
      | Rule (i',p) ->
        if Int.equal i i' then
          true
        else
          simple_useless_rule
            p
      | _ -> false
    end
  in
  begin match p with
    | Or ps ->
      multi_or
        (List.map
           ~f:(fun p -> rule (i,p))
           ps)
    | _ ->
      if simple_useless_rule p then
        p
      else
        Rule (i,p)
    end*)

let rec to_rule_list_list (p : t) : int list list =
  match p with
  | Or ps -> List.concat_map ~f:to_rule_list_list ps
  | And ps ->
    let rs = List.map ~f:to_rule_list_list ps in
    let combined = combinations rs in
    List.map ~f:List.concat combined
  | Rule i -> [[i]]
;;

let show_with_structure : t -> string = show

let show (p : t) : string =
  let provLists = to_rule_list_list p in
  let rec show_single_helper (l : int list) =
    match l with
    | [] -> ""
    | [hd] -> "," ^ string_of_int hd ^ "]"
    | hd :: tl -> "," ^ string_of_int hd ^ show_single_helper tl
  in
  let show_single (l1 : int list) = "[" ^ show_single_helper l1 ^ "]" in
  let rec helper (l : int list list) =
    match l with
    | [] -> "\n"
    | hd :: tl -> show_single hd ^ "\n" ^ helper tl
  in
  helper provLists
;;

let print (p : t) : unit = print_endline (show p)

let single_prov (p : t) : IntSet.t option =
  let rec aux (current : IntSet.t option) (pr : t) : IntSet.t option =
    match current with
    | None -> None
    | Some c ->
      (match pr with
      | Or tList ->
        (match tList with
        | [] -> None
        | hd :: tl ->
          (match aux current hd with
          | None -> aux current (Or tl)
          | Some derivation -> Some (IntSet.union c derivation)))
      | And tList ->
        let pieces = List.map ~f:(fun pr2 -> aux current pr2) tList in
        (* Combines subprovenances 
         * so that if one of the pieces is underivable,
         * no parse is found. *)
        let rec smart_combine subprovs running =
          match running with
          | None -> None
          | Some r ->
            (match subprovs with
            | [] -> running
            | hd :: tl ->
              (match hd with
              | None -> None
              | Some sp ->
                let combined = Some (IntSet.union r sp) in
                smart_combine tl combined))
        in
        smart_combine pieces (Some c)
      | Rule i -> Some (IntSet.insert i c))
  in
  aux (Some IntSet.empty) p
;;

let from_rule_rule_list (rrl : int list list) : t =
  multi_or
    (List.map ~f:(fun rl -> multi_and (List.map ~f:(fun i -> rule i) rl)) rrl)
;;

let extract_novel_rules (pr1 : t) (pr2 : t) : t =
  let sort_rrl rrl = List.map ~f:(List.sort ~compare:Int.compare) rrl in
  let pr1_rrl = sort_rrl (to_rule_list_list pr1) in
  let pr2_rrl = sort_rrl (to_rule_list_list pr2) in
  let rrl =
    List.filter
      ~f:(fun pr2 ->
        not
          (List.exists
             ~f:(fun pr1 -> sublist_on_sorted ~cmp:Int.compare pr1 pr2)
             pr1_rrl))
      pr2_rrl
  in
  from_rule_rule_list rrl
;;

let rec is_zero (pr : t) : bool =
  match pr with
  | Or prs -> List.for_all ~f:is_zero prs
  | And prs -> List.exists ~f:is_zero prs
  | Rule i -> false
;;

let rec extract_accepting_path (pr : t) : int list option =
  match pr with
  | Or (hpr :: prt) ->
    begin
      match extract_accepting_path hpr with
      | None -> extract_accepting_path (Or prt)
      | Some ap -> Some ap
    end
  | Or [] -> None
  | And prs ->
    Option.map
      ~f:List.concat
      (distribute_option (List.map ~f:extract_accepting_path prs))
  | Rule i -> Some [i]
;;

(*let rec compress
    (p:t)
  : t =
  begin match p with
    | Or ps -> compress_multi_or (List.map ~f:compress ps)
    | And ps -> compress_multi_and (List.map ~f:compress ps)
    | _ -> p
  end*)
