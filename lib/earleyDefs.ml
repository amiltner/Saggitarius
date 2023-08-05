open MyStdLib

module SymOption = struct
  type t = Symbol.t option [@@deriving hash, eq, compare, show]
end

(**** EarleyItem Module ****)
module EarleyItem = struct
  type t =
    { parsed : Production.t [@ignore]
    ; start : int
    ; head : NumberedId.t
    ; unparsed : Production.t
    ; prov : ProvRecorder.t [@ignore]
          (* I don't want this field to affect hashes or compares.*)
    }
  [@@deriving hash]

  let union_prov (ea1 : t) (ea2 : t) : t =
    { ea1 with prov = ProvRecorder.or_ ea1.prov ea2.prov }
  ;;

  let compare (a : t) (b : t) =
    let c1 = NumberedId.compare a.head b.head in
    if not (phys_equal c1 0)
    then c1
    else (
      let c2 = Int.compare a.start b.start in
      if not (phys_equal c2 0)
      then c2
      else (
        let c3 = Production.compare a.unparsed b.unparsed in
        c3))
  ;;

  let equal (a : t) (b : t) = compare a b = 0

  let create ~par:p ~unpar:u ~start:s ~head:h ~prov:pr : t =
    { parsed = p; unparsed = u; start = s; head = h; prov = pr }
  ;;

  let show x : string =
    NumberedId.show x.head
    ^ " \t -> "
    ^ Production.show (List.rev x.parsed)
    ^ " <---> "
    ^ Production.show x.unparsed
    ^ " \t ("
    ^ string_of_int x.start
    ^ ") \n"
    (*"\tProv:" ^ (ProvList.show x.prov)*)
    ^ "\tProv:\n"
    ^ ProvRecorder.show x.prov
  ;;

  (*"\tProv:" ^ (ProvRecorder.quick_show x.prov)*)

  let getName (x : t) : string = Nts.show x.head.id
  let pp (fmt : Format.formatter) (x : t) = Format.pp_print_string fmt (show x)

  let advance ?(prov : ProvRecorder.t = ProvRecorder.one) (x : t) : t option =
    match x.unparsed with
    | [] -> None
    | hd :: tl ->
      Some
        (create
           ~par:(hd :: x.parsed)
           ~unpar:tl
           ~start:x.start
           ~head:x.head
           ~prov:(ProvRecorder.and_ prov x.prov))
  ;;

  let get_nu_symopt (i : t) : SymOption.t =
    match i.unparsed with
    | [] -> None
    | hd :: _ -> Some hd
  ;;
end

(**** EarleySet Module ****)
exception DuplicateMatches

module EarleySet = struct
  include DictOf (EarleyItem) (EarleyItem)

  let insert (k : EarleyItem.t) (s : t) : t =
    insert_or_combine ~combiner:EarleyItem.union_prov s k k
  ;;

  let as_list = List.map ~f:snd % as_kvp_list

  let show x =
    let rec show_helper itemslist =
      match itemslist with
      | [] -> ""
      | hd :: tl -> EarleyItem.show hd ^ "\n" ^ show_helper tl
    in
    show_helper (as_list x)
  ;;

  let get_matching (x : t) (i : EarleyItem.t) : EarleyItem.t option =
    match List.filter ~f:(EarleyItem.equal i) (as_list x) with
    | [] -> None
    | [ei] -> Some ei
    | hd :: tl -> raise DuplicateMatches
  ;;
end

exception EmptyLookup

module EarleyDict = struct
  include DictOf (SymOption) (EarleySet)

  let show (d : t) : string =
    let allsets = as_kvp_list d in
    let strings =
      List.map
        ~f:(fun (ko, v) ->
          let kos = SymOption.show ko in
          let vs = EarleySet.show v in
          kos ^ " HAS THE SET\n" ^ vs)
        allsets
    in
    String.concat ~sep:"\n" strings
  ;;

  (*let combined = List.fold_left allsets ~init:EarleySet.empty ~f:EarleySet.union in *)
  (*EarleySet.show combined*)

  let remove_item_from_set (d : t) (item : EarleyItem.t) : t =
    let nid = EarleyItem.get_nu_symopt item in
    match lookup d nid with
    | None -> raise EmptyLookup
    | Some es ->
      let res = EarleySet.remove es item in
      insert d nid res
  ;;

  let member_item (d : t) (item : EarleyItem.t) : bool =
    let nid = EarleyItem.get_nu_symopt item in
    match lookup d nid with
    | None -> false
    | Some es -> EarleySet.member es item
  ;;

  let insert_item (d : t) (item : EarleyItem.t) : t =
    let nid = EarleyItem.get_nu_symopt item in
    match lookup d nid with
    | None ->
      let ns = EarleySet.insert item EarleySet.empty in
      insert d nid ns
    | Some es ->
      let ns = EarleySet.insert item es in
      insert d nid ns
  ;;

  let get_item (d : t) (item : EarleyItem.t) : EarleyItem.t option =
    let nid = EarleyItem.get_nu_symopt item in
    match lookup d nid with
    | None -> None
    | Some es -> EarleySet.get_matching es item
  ;;

  let all_values (d : t) : EarleyItem.t list =
    let allsets = List.map ~f:snd (as_kvp_list d) in
    let allvals = List.map ~f:EarleySet.as_list allsets in
    List.concat allvals
  ;;
end

module EarleyChart = struct
  include DictOf (IntModule) (EarleyDict)

  let showkvp (qi, bs) =
    "---------" ^ IntModule.show qi ^ "---------\n" ^ EarleyDict.show bs ^ "\n"
  ;;

  let keycompare (k1, _) (k2, _) = IntModule.compare k1 k2

  exception KeyError

  let remove_item (key : IntModule.t) (item : EarleyItem.t) (ec : t) =
    match lookup ec key with
    | None -> raise KeyError
    | Some ed ->
      let ns = EarleyDict.remove_item_from_set ed item in
      insert ec key ns
  ;;

  let show x =
    let rec helper kvps =
      match kvps with
      | [] -> ""
      | hd :: tl -> showkvp hd ^ helper tl
    in
    helper (List.sort ~compare:keycompare (as_kvp_list x))
  ;;
end
