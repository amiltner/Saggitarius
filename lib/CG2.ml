open MyStdLib
open ED2
open NtsToNumber
module NtsSet = Stdlib.Set.Make (Nts)

module NullablesDict = struct
  include DictOf (Nts) (ProvRecorder)

  let lookup d v =
    match lookup d v with
    | None -> ProvRecorder.zero
    | Some pr -> pr
  ;;
end

exception EmptyValue

(*Earley Parsing works better when rules are written
 * out individually (as opposed to EBNF) *)
module GrammarSpec = struct
  include DictOf (NumberedId) (Production)
  module NIDS = SetOf (NumberedId)

  (**** Creates a nts to number mapping ****)
  let nts_to_number_map (g : t) : NtsToNumber.t =
    let add_next (running : NtsToNumber.t) (nid : NumberedId.t) : NtsToNumber.t =
      let key = nid.id in
      let value = nid.idx in
      match NtsToNumber.lookup running key with
      | None -> NtsToNumber.insert running key [value]
      | Some qintlist ->
        let inew = value :: qintlist in
        NtsToNumber.insert running key inew
    in
    let klist = key_list g in
    List.fold ~init:NtsToNumber.empty ~f:add_next klist
  ;;

  (**** Useful functions for pulling values given an ntsmap ****)
  (* kvps from a collection of ntses*)
  let pairs_from_ntslist (g : t) ntsmap (l : NumberedId.t list)
      : (NumberedId.t * Production.t) list
    =
    List.filter_map
      ~f:(fun x ->
        match lookup g x with
        | None -> None
        | Some v -> Some (x, v))
      l
  ;;

  let pairs_from_ntslist_plain (g : t) (l : NumberedId.t list) =
    let ntsmap = nts_to_number_map g in
    pairs_from_ntslist g ntsmap l
  ;;

  (* kvps from a single nts*)
  let pairs_from_nts (g : t) ntsmap (nts : Nts.t)
      : (NumberedId.t * Production.t) list
    =
    let uk = NtsToNumber.lookup_default_nid ntsmap nts in
    pairs_from_ntslist g ntsmap uk
  ;;

  let pairs_from_nts_plain (g : t) (nts : Nts.t)
      : (NumberedId.t * Production.t) list
    =
    let ntsmap = nts_to_number_map g in
    pairs_from_nts g ntsmap nts
  ;;

  (**** Useful functions for pulling all integer numbers for rules
   * that match a given description ****)
  let filter_keys (f : NumberedId.t -> bool) (g : t) : int list =
    let res = List.filter ~f (key_list g) in
    List.map ~f:(fun nid -> nid.idx) res
  ;;

  let filter_by_nts (nts : Nts.t) (g : t) : int list =
    let f (nid : NumberedId.t) = Nts.equal nts nid.id in
    filter_keys f g
  ;;

  let filter_by_name (name : Id.t) (g : t) : int list =
    let f (nid : NumberedId.t) =
      let name, _ = nid.id in
      Id.equal name name
    in
    filter_keys f g
  ;;

  exception TooManyMatches

  let filter_by_name_idxs (name : Id.t) (idxs : int list) (g : t) : int option =
    let f (nid : NumberedId.t) =
      let n1, idxs1 = nid.id in
      Id.equal n1 name && List.equal ( = ) idxs idxs1
    in
    match filter_keys f g with
    | [] -> None
    | [i] -> Some i
    | hd :: tl -> raise TooManyMatches
  ;;

  (**** These functions create grammars ****)

  (*Functions that help select subgrammars*)
  let subgrammar (supergrammar : t) (ruleslist : NumberedId.t list) : t =
    let rec aux (l : NumberedId.t list) (g : t) : t =
      match l with
      | [] -> g
      | key :: tl ->
        let value =
          match lookup supergrammar key with
          | None -> raise EmptyValue
          | Some v -> v
        in
        aux tl (insert g key value)
    in
    aux ruleslist empty
  ;;

  let subgrammar_prov (supergrammar : t) (rulesprov : RuleSet.t) : t =
    subgrammar supergrammar (RuleSet.as_list rulesprov)
  ;;

  (*Builds a complementary grammar from a list of excluded rules*)
  let complement_grammar (supergrammar : t) (excluded : NumberedId.t list) : t =
    let rec aux (l : NumberedId.t list) (g : t) : t =
      match l with
      | [] -> g
      | key :: tl ->
        (match lookup supergrammar key with
        | None -> raise EmptyValue
        | Some _ -> aux tl (remove g key))
    in
    aux excluded supergrammar
  ;;

  let complement_prov (supergrammar : t) (excludedprov : RuleSet.t) : t =
    complement_grammar supergrammar (RuleSet.as_list excludedprov)
  ;;

  (*** These functions print grammars ***)
  let showkvp (nid, prod) =
    NumberedId.show nid ^ "\t -> " ^ Production.show prod
  ;;

  let show (g : t) =
    let rec aux rest =
      match rest with
      | [] -> ""
      | hd :: tl -> showkvp hd ^ "\n" ^ aux tl
    in
    aux
      (List.sort (as_kvp_list g) (fun (k1, _) (k2, _) ->
           NumberedId.compare k1 k2))
  ;;

  let pp (fmt : Format.formatter) (x : t) = Format.pp_print_string fmt (show x)

  (**** These functions calculate which symbols have certain properties ****)
  let reachable (g : t) (ntsToNum : NtsToNumber.t) (startNts : Nts.t) : t =
    (*Function to calculate what subgrammar is reachable from a starting symbol*)
    let rec bfs (stack : Production.t list) (marked : RuleSet.t) =
      let rec process_item
          (prod : Production.t)
          (st : Production.t list)
          (mk : RuleSet.t)
          : Production.t list * RuleSet.t
        =
        match prod with
        | [] -> st, mk
        | Terminal _ :: tl -> process_item tl st mk
        | Nonterminal nts :: tl ->
          let uk = pairs_from_nts g ntsToNum nts in
          let test ((nid, prod) : NumberedId.t * Production.t) =
            not (RuleSet.member mk nid)
          in
          let newNidProds = List.filter ~f:test uk in
          let newStack =
            List.fold ~init:st ~f:(fun a (_, pr) -> pr :: a) newNidProds
          in
          let newMarked =
            List.fold
              ~init:mk
              ~f:(fun m (nid, _) -> RuleSet.insert nid m)
              newNidProds
          in
          process_item tl newStack newMarked
      in
      match stack with
      | [] -> subgrammar_prov g marked
      | hd :: tl ->
        let s, m = process_item hd tl marked in
        bfs s m
    in
    let startPairs = pairs_from_nts g ntsToNum startNts in
    let startProds = List.map ~f:snd startPairs in
    bfs startProds RuleSet.empty
  ;;

  (* if ntsToNumber hasn't already been calculated *)
  let reachable_plain (g : t) (startNts : Nts.t) : t =
    let ntn = nts_to_number_map g in
    reachable g ntn startNts
  ;;

  let nullable_symbols (g : t) : NtsSet.t =
    let rec aux (oldset : NtsSet.t) =
      let kvpList = as_kvp_list g in
      let is_nullable (prod : Production.t) =
        (*Note that this for_all will be true for null production as desired*)
        List.for_all
          ~f:(fun sym ->
            match sym with
            | Symbol.Terminal _ -> false
            | Nonterminal nts -> NtsSet.mem nts oldset)
          prod
      in
      let nullables =
        List.filter_map
          ~f:(fun (nidkey, prodvalue) ->
            if is_nullable prodvalue then Some nidkey.id else None)
          kvpList
      in
      let newterms =
        List.filter ~f:(fun name -> not (NtsSet.mem name oldset)) nullables
      in
      if List.length newterms = 0
      then oldset
      else (
        let newset =
          List.fold_left
            ~f:(fun set nts -> NtsSet.add nts set)
            ~init:oldset
            newterms
        in
        aux newset)
    in
    let result = aux NtsSet.empty in
    result
  ;;

  let nullable_prov (g : t) : NullablesDict.t =
    let kvpList = as_kvp_list g in
    let rec aux (oldset : NullablesDict.t) =
      let extract_prov (prod : Production.t) =
        (*Note that this for_all will be true for null production as desired*)
        List.map
          ~f:(fun sym ->
            match sym with
            | Symbol.Terminal _ -> ProvRecorder.zero
            | Nonterminal nts -> NullablesDict.lookup oldset nts)
          prod
      in
      let provs =
        List.filter_map
          ~f:(fun (nidkey, prodvalue) ->
            let prov = ProvRecorder.multi_and (extract_prov prodvalue) in
            if ProvRecorder.is_zero prov
            then None
            else
              Some
                ( nidkey.id
                , ProvRecorder.and_ (ProvRecorder.rule nidkey.idx) prov ))
          kvpList
      in
      let added_new = ref false in
      let updated =
        List.fold
          ~f:(fun d (k, pr) ->
            NullablesDict.insert_or_combine
              ~combiner:(fun pr1 pr2 ->
                let nrs = ProvRecorder.extract_novel_rules pr1 pr2 in
                if ProvRecorder.is_zero nrs
                then pr1
                else (
                  added_new := true;
                  ProvRecorder.or_ pr1 nrs))
              d
              k
              pr)
          ~init:oldset
          provs
      in
      if !added_new then aux updated else oldset
    in
    let saturated_empty =
      NullablesDict.from_kvp_list
        (List.map ~f:(fun (i, _) -> i.id, ProvRecorder.zero) kvpList)
    in
    aux saturated_empty
  ;;

  let is_nullable (item : EarleyItem.t) (nullables : NtsSet.t) : bool =
    NtsSet.mem item.head.id nullables
  ;;

  (*** Helper functions for Creating Constraints***)
  let cnstr_from_name (name : Id.t) (g : t) : Constraint.t =
    let il = filter_by_name name g in
    let idxs = List.map ~f:(fun i -> Constraint.ProdId i) il in
    Constraint.And idxs
  ;;

  let cnstr_from_names (names : Id.t list) (g : t) : Constraint.t =
    let il = List.concat (List.map ~f:(fun x -> filter_by_name x g) names) in
    let idxs = List.map ~f:(fun i -> Constraint.ProdId i) il in
    Constraint.And idxs
  ;;

  exception Impossible

  (*[n1;n2;n3] becomes N1 -> N2 -> N3*)
  let cnstr_from_names_idxs (name : Id.t) (idxs : int list list) (g : t)
      : Constraint.t list
    =
    let ruleNums : int list =
      List.filter_map
        ~f:(fun (idx : int list) -> filter_by_name_idxs name idx g)
        idxs
    in
    match ruleNums with
    | [] -> []
    | [_] -> []
    | _ :: ahead ->
      let rec c_maker (b : int list) (a : int list) : Constraint.t list =
        match b, a with
        | [], _ -> raise Impossible
        | _ :: _, [] -> []
        | hd1 :: tl1, [hd2] ->
          let x = Constraint.ProdId hd1 in
          let y = Constraint.ProdId hd2 in
          let c = Constraint.Implies (x, y) in
          [c]
        | hd1 :: tl1, hd2 :: tl2 ->
          let x = Constraint.ProdId hd1 in
          let y = Constraint.ProdId hd2 in
          let c = Constraint.Implies (x, y) in
          c :: c_maker tl1 tl2
      in
      c_maker ruleNums ahead
  ;;

  (* I lied this file is also going to contain CSV constraints *)
  let group_names (lazynames : string list) (g : t) : Constraint.t =
    let ids = List.map ~f:Id.create lazynames in
    cnstr_from_names ids g
  ;;

  let imply_names ((dep, reqs) : string * string list) (g : t) : Constraint.t =
    let ids = List.map ~f:Id.create reqs in
    let il = List.concat (List.map ~f:(fun x -> filter_by_name x g) ids) in
    let idxs = List.map ~f:(fun i -> Constraint.ProdId i) il in
    let drules = filter_by_name (Id dep) g in
    let dIdxs = List.map ~f:(fun i -> Constraint.ProdId i) drules in
    Constraint.Implies (Constraint.Or dIdxs, Constraint.All idxs)
  ;;
end
