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
    ; prov : ProvList.t [@ignore]
          (* I don't want this field to affect hashes or compares.*)
    }
  [@@deriving hash, ord]

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
    ^ "\tProv:"
    ^ ProvList.show x.prov
  ;;

  let getName (x : t) : string = Nts.show x.head.id
  let pp (fmt : Format.formatter) (x : t) = Format.pp_print_string fmt (show x)

  let advance_add_prov (x : t) (prov : ProvList.t) : t option =
    match x.unparsed with
    | [] -> None
    | hd :: tl ->
      Some
        (create
           ~par:(hd :: x.parsed)
           ~unpar:tl
           ~start:x.start
           ~head:x.head
           ~prov:(prov @ x.prov))
  ;;

  let advance (x : t) : t option =
    match x.unparsed with
    | [] -> None
    | hd :: tl ->
      Some
        (create
           ~par:(hd :: x.parsed)
           ~unpar:tl
           ~start:x.start
           ~head:x.head
           ~prov:x.prov)
  ;;

  let get_nu_symopt (i : t) : SymOption.t =
    match i.unparsed with
    | [] -> None
    | hd :: _ -> Some hd
  ;;
end

(**** EarleySet Module ****)
module EarleySet = struct
  include SetOf (EarleyItem)

  let show x =
    let rec show_helper itemslist =
      match itemslist with
      | [] -> ""
      | hd :: tl -> EarleyItem.show hd ^ "\n" ^ show_helper tl
    in
    show_helper (as_list x)
  ;;
end

exception EmptyLookup

module EarleyDict = struct
  include DictOf (SymOption) (EarleySet)

  let show (d : t) =
    let values = value_list d in
    let merged = List.fold ~f:EarleySet.union ~init:EarleySet.empty values in
    EarleySet.show merged
  ;;

  let remove_item_from_set (d : t) (item : EarleyItem.t) : t =
    let nid = EarleyItem.get_nu_symopt item in
    match lookup d nid with
    | None -> raise EmptyLookup
    | Some es ->
      let res = EarleySet.remove item es in
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
