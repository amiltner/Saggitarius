open MyStdLib

type t =
  | BigQ of RangeBinding.t * t list
  | Singleton of bool * BoolExp.t * IndexedId.t option * Regex.t
[@@deriving eq, hash, ord, show]

let rec update_ranges_from_symbolic
    (id:Id.t)
    (lr : int)
    (hr : int)
    (s:t)
  : t =
  begin match s with
    | BigQ ((id',(b1,ie1,b2,ie2)),ss) ->
      let ie1 = Exp.int_exp_replace_var_with id lr ie1 in
      let ie2 = Exp.int_exp_replace_var_with id hr ie2 in
      let ss = List.map ~f:(update_ranges_from_symbolic id lr hr) ss in
      BigQ ((id',(b1,ie1,b2,ie2)),ss)
    | Singleton _ -> s
  end

let rec update_ranges_from_symbolics
    (symbolics:(Id.t * int * int) list)
    (s:t)
  : t =
  List.fold
    ~f:(fun acc (i,lr,hr) -> update_ranges_from_symbolic i lr hr acc)
    ~init:s
    symbolics

let rec size (s : t) : int =
  match s with
  | BigQ (rb, s) ->
    let rbs = RangeBinding.size rb in
    let ss = List.map ~f:size s in
    1 + rbs + sum_int_list ss
  | Singleton (_, be, iido, rx) ->
    let iids =
      match iido with
      | None -> 0
      | Some iid -> IndexedId.size iid
    in
    let bes = BoolExp.size be in
    let rxs = Regex.size rx in
    1 + iids + bes + rxs
;;

let rec prod_size (s : t) : int =
  match s with
  | BigQ (rb, s) ->
    let ss = List.map ~f:prod_size s in
    sum_int_list ss
  | Singleton _ ->
    1
;;

let rec replace_infty_with (i : int) (s : t) : t =
  match s with
  | BigQ (rb, ss) ->
    BigQ
      ( RangeBinding.replace_infty_with i rb
      , List.map ~f:(replace_infty_with i) ss )
  | Singleton (b, be, iido, r) ->
    Singleton
      ( b
      , BoolExp.replace_infty_with i be
      , Option.map ~f:(IndexedId.replace_infty_with i) iido
      , Regex.replace_infty_with i r )
;;

let rec contains_nonranged_var (i:Id.t) (s:t) : bool =
  match s with
  | BigQ (_, ss) ->
    List.exists ~f:(contains_nonranged_var i) ss
  | Singleton (_, _, _, r) ->
    Regex.contains_var i r
;;

let rec update_callsites_if_needed_from_symbolics
    (updated_prods:(Id.t * int * int) list)
    (space:t)
  : t =
  begin match space with
    | BigQ (rb,ss) ->
      BigQ
        (rb,
         List.map
           ~f:(update_callsites_if_needed_from_symbolics updated_prods)
           ss)
    | Singleton (b,bexp,iido,rx) ->
      let autogen i =
        Id.create ("symbolics_autogen" ^ (Int.to_string i))
      in
      let (rx,calls,_) =
        List.fold
          ~init:(rx,[],0)
          ~f:(fun (rx,calls,count) (id,callsite_low,callsite_high) ->
              if Regex.contains_prod id rx then
                let new_id = autogen count in
                let count = count + 1 in
                let calls = (new_id,callsite_low,callsite_high)::calls in
                let rx = Regex.add_prod_app id new_id rx in
                (rx,calls,count)
              else
                (rx,calls,count))
          updated_prods
      in
      List.fold
        ~f:(fun acc (i,l,r) ->
            BigQ
              ((i,(Exp.Inclusive,Exp.Int l,Exp.Exclusive,Exp.Int r))
              ,[acc]))
        ~init:(Singleton (b,bexp,iido,rx))
        calls
  end

let rec to_il
    (env : (Id.t * int) list)
    (acc : ConversionAcc.t)
    (s : t)
    (nt : Nts.t)
  : ConversionAcc.t
  =
  match s with
  | Singleton (b1, b2, iido, r) ->
    if BoolExp.eval env b2
    then (
      let acc = ConversionAcc.add_name_at_index env acc iido in
      let acc =
        ConversionAcc.add_concretized
          acc
          acc.index
          nt
          (Regex.to_concretized_regex env r)
      in
      Regex.to_il b1 env acc r nt)
    else acc
  | BigQ ((i, r), cs) ->
    let i1, i2 = Exp.eval_range env r in
    List.fold
      ~f:(fun acc v ->
          let env = (i, v) :: env in
          List.fold ~f:(fun acc c -> to_il env acc c nt) ~init:acc cs)
      ~init:acc
      (range i1 i2)
;;
