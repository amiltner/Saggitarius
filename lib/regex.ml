open MyStdLib

type t =
  | CharSet of CharRange.CRList.t
  | Disjunct of t list
  | Conjunct of t list
  | Star of t
  | Nonterminal of IndexedId.t
[@@deriving eq, hash, ord, show]

let is_singleton_crlist (crl : t) : bool =
  match crl with
  | CharSet crl -> CharRange.CRList.is_singleton crl
  | _ -> false
;;

let rec size (r : t) : int =
  match r with
  | CharSet crs -> 1 + CharRange.CRList.size crs
  | Disjunct rl ->
    let rs = List.map ~f:size rl in
    1 + sum_int_list rs
  | Conjunct rl ->
    if List.for_all ~f:is_singleton_crlist rl
    then 1
    else (
      let rs = List.map ~f:size rl in
      1 + sum_int_list rs)
  | Star r ->
    let s = size r in
    1 + s
  | Nonterminal iid -> IndexedId.size iid
;;

let disjunct (rs : t list) : t =
  let rec flatten (p : t) : t list =
    match p with
    | Disjunct ps -> List.concat_map ~f:flatten ps
    | _ -> [p]
  in
  let flattened = List.concat_map ~f:flatten rs in
  let by_crlist =
    List.map
      ~f:(fun p ->
        match p with
        | CharSet crl -> Left crl
        | _ -> Right p)
      flattened
  in
  let crls, others = split_by_either by_crlist in
  let crs = List.concat crls in
  let crl = CharRange.CRList.compress crs in
  let flattened =
    match crl with
    | [] -> others
    | _ -> CharSet crl :: others
  in
  match flattened with
  | [h] -> h
  | _ -> Disjunct flattened
;;

let fold
    (type a)
    ~(char_set : CharRange.CRList.t -> a)
    ~(disjunct : a list -> a)
    ~(conjunct : a list -> a)
    ~(star : a -> a)
    ~(nonterminal : Id.t -> IntExp.t list -> a)
    : t -> a
  =
  let rec fold_internal (r : t) : a =
    match r with
    | CharSet crs -> char_set crs
    | Disjunct ds -> disjunct (List.map ~f:fold_internal ds)
    | Conjunct cs -> conjunct (List.map ~f:fold_internal cs)
    | Star r -> star (fold_internal r)
    | Nonterminal (i, indices) -> nonterminal i indices
  in
  fold_internal
;;

let replace_infty_with i =
  fold
    ~char_set:(fun cs -> CharSet cs)
    ~disjunct:(fun ds -> Disjunct ds)
    ~conjunct:(fun ds -> Conjunct ds)
    ~star:(fun s -> Star s)
    ~nonterminal:(fun id ies ->
      Nonterminal (IndexedId.replace_infty_with i (id, ies)))
;;

let contains_var i =
  fold
    ~char_set:(fun _ -> false)
    ~disjunct:(fun ds -> List.exists ~f:ident ds)
    ~conjunct:(fun ds -> List.exists ~f:ident ds)
    ~star:ident
    ~nonterminal:(fun id ies ->
        (IndexedId.contains_var i (id,ies)))

let contains_prod i =
  fold
    ~char_set:(fun _ -> false)
    ~disjunct:(fun ds -> List.exists ~f:ident ds)
    ~conjunct:(fun ds -> List.exists ~f:ident ds)
    ~star:ident
    ~nonterminal:(fun id ies -> Id.equal i id)

let add_prod_app (nt:Id.t) (int_id:Id.t) =
  fold
    ~char_set:(fun cs -> CharSet cs)
    ~disjunct:(fun ds -> Disjunct ds)
    ~conjunct:(fun ds -> Conjunct ds)
    ~star:(fun v -> Star v)
    ~nonterminal:(fun id ies ->
        if Id.equal id nt then
          Nonterminal (id,ies@[Exp.Var int_id])
        else
          Nonterminal (id,ies))

let to_concretized_regex (env : (Id.t * int) list) : t -> ConcretizedRegex.t =
  fold
    ~char_set:ConcretizedRegex.charset
    ~disjunct:ConcretizedRegex.disjunct
    ~conjunct:ConcretizedRegex.conjunct
    ~star:ConcretizedRegex.star
    ~nonterminal:(fun i is ->
      ConcretizedRegex.nonterminal (i, List.map ~f:(IntExp.eval env) is))
;;

let rec to_symbol_list (env : (Id.t * int) list) (acc : ConversionAcc.t) (r : t)
    : Symbol.t list * ConversionAcc.t
  =
  match r with
  | CharSet cs -> [Symbol.Terminal cs], acc
  | Conjunct rs ->
    List.fold_right
      ~f:(fun r (ss, acc) ->
        let ss', acc = to_symbol_list env acc r in
        ss' @ ss, acc)
      ~init:([], acc)
      rs
  | Disjunct _ ->
    let nt = Id.from_int acc.index, [] in
    let acc = ConversionAcc.increment_index acc in
    let acc = to_ip_list env acc r nt in
    [Symbol.Nonterminal nt], acc
  | Star r ->
    let nt = Id.from_int acc.index, [] in
    let r = Disjunct [Conjunct []; Conjunct [r; Nonterminal nt]] in
    let acc = to_ip_list env acc r nt in
    [Symbol.Nonterminal nt], acc
  | Nonterminal (id, is) ->
    let vs = List.map ~f:(Exp.eval_int_exp env) is in
    let nt = id, vs in
    [Symbol.Nonterminal nt], acc

and to_ip_list
    (env : (Id.t * int) list)
    (acc : ConversionAcc.t)
    (r : t)
    (nt : Nts.t)
    : ConversionAcc.t
  =
  match r with
  | Disjunct rs ->
    List.fold ~f:(fun acc r -> to_ip_list env acc r nt) ~init:acc rs
  | Star _ | CharSet _ | Conjunct _ | Nonterminal _ ->
    let pid = acc.index in
    let acc = ConversionAcc.increment_index acc in
    let ss, acc = to_symbol_list env acc r in
    let new_ip =
      IndexedProduction.make ~pid ~required:true ~head:nt ~symbols:ss
    in
    ConversionAcc.add_ip acc new_ip
;;

let to_il
    (reqd : bool)
    (env : (Id.t * int) list)
    (acc : ConversionAcc.t)
    (r : t)
    (nt : Nts.t)
    : ConversionAcc.t
  =
  let pid = acc.index in
  let acc = ConversionAcc.increment_index acc in
  let ss, acc = to_symbol_list env acc r in
  let new_ip =
    IndexedProduction.make ~pid ~required:reqd ~head:nt ~symbols:ss
  in
  ConversionAcc.add_ip acc new_ip
;;
