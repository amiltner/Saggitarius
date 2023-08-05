open MyStdLib

type t =
  { head : Id.t * RangeBinding.t list
  ; spaces : Space.t list
  }
[@@deriving eq, hash, ord, show]

let size (mp : t) : int =
  let _, rbs = mp.head in
  let rbss = List.map ~f:RangeBinding.size rbs in
  let ss = List.map ~f:Space.size mp.spaces in
  let rs = sum_int_list rbss in
  let ss = sum_int_list ss in
  rs + ss + 1
;;

let prod_size (mp:t)
  : int =
  let ss = List.map ~f:Space.prod_size mp.spaces in
  sum_int_list ss

let update_ranges_from_symbolics
    (symbolics:(Id.t * int * int) list)
    (mp:t)
  : t =
  let spaces =
    List.map
      ~f:(fun s -> Space.update_ranges_from_symbolics symbolics s)
      mp.spaces
  in
  let (id,rbs) = mp.head in
  let rbs =
    List.map
      ~f:(fun (i,rb) ->
          let rb =
            List.fold
              ~f:(fun (bl,ie1,br,ie2) (id,lr,hr) ->
                  let ie1 = Exp.int_exp_replace_var_with id lr ie1 in
                  let ie2 = Exp.int_exp_replace_var_with id hr ie2 in
                  (bl,ie1,br,ie2))
              ~init:rb
              symbolics
          in
          (i,rb))
      rbs
  in
  let head = (id,rbs) in
  { head ;
    spaces ;
  }

let replace_infty_with (i : int) (mp : t) : t =
  let id, rbs = mp.head in
  { head = id, List.map ~f:(RangeBinding.replace_infty_with i) rbs
  ; spaces = List.map ~f:(Space.replace_infty_with i) mp.spaces
  }
;;

let contains_nonranged_var (i : Id.t) (mp : t) : bool =
  List.exists ~f:(Space.contains_nonranged_var i) mp.spaces
;;

let update_args_if_needed_from_symbolic
    (id:Id.t)
    (lr:int)
    (hr:int)
    (mp:t)
  : t * bool =
  if contains_nonranged_var id mp then
    let (prodid,rbs) = mp.head in
    let mp =
      { mp with
        head = (prodid,rbs@[(id,(Exp.Inclusive,Exp.Int lr,Exp.Exclusive,Exp.Int hr))])
      }
    in
    (mp,true)
  else
    (mp,false)

let update_args_if_needed_from_symbolics
    (symbolics:(Id.t * int * int) list)
    (mp:t)
  : t * ((Id.t * int * int) list) =
  let arg_id = fst mp.head in
  List.fold_right
    ~f:(fun (id,lr,hr) (mp,ids) ->
        let (mp,updated) = update_args_if_needed_from_symbolic id lr hr mp in
        let ids = if updated then (arg_id,lr,hr)::ids else ids in
        (mp,ids))
    ~init:(mp,[])
    symbolics

let update_callsites_if_needed_from_symbolics
    (updated_prods:(Id.t * int * int) list)
    (mp:t)
  : t =
  { mp with
    spaces =
      List.map
        ~f:(Space.update_callsites_if_needed_from_symbolics updated_prods)
        mp.spaces
  }


let to_il (acc : ConversionAcc.t) (mp : t) : ConversionAcc.t =
  let spaces = mp.spaces in
  let head_id, bindings = mp.head in
  let rec to_il
      (env : (Id.t * int) list)
      (acc : ConversionAcc.t)
      (bindings : RangeBinding.t list)
      (args : int list)
      : ConversionAcc.t
    =
    match bindings with
    | [] ->
      List.fold
        ~f:(fun a s -> Space.to_il env a s (head_id, List.rev args))
        ~init:acc
        spaces
    | (i, r) :: t ->
      let lower, upper = Exp.eval_range env r in
      let vals = range lower upper in
      List.fold
        ~f:(fun acc v ->
          let env = (i, v) :: env in
          to_il env acc t (v :: args))
        ~init:acc
        vals
  in
  to_il [] acc bindings []
;;

let make ~(head : Id.t * RangeBinding.t list) ~(spaces : Space.t list) : t =
  { head; spaces }
;;
