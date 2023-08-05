open MyStdLib

type t = Exp.int_exp [@@deriving eq, hash, ord, show]

let eval = Exp.eval_int_exp

let rec to_sum_exn
    (env : (Id.t * int) list)
    (d : (Nts.t * int) list)
    (pm : (Nts.t * int list) list)
    (ie : t)
    : Exp.bool_exp list
  =
  match ie with
  | ITE (b, Int 1, Int 0) -> [Exp.concretize_indices env b]
  | Plus (i1, i2) -> to_sum_exn env d pm i1 @ to_sum_exn env d pm i2
  | SumOver ((i, r), ie) ->
    let i1, i2 = Exp.eval_range env r in
    let vs = range i1 i2 in
    let subs = List.map ~f:(fun v -> to_sum_exn ((i, v) :: env) d pm ie) vs in
    List.concat subs
  | SizeOf iil ->
    let is = Exp.to_int_list env d pm iil in
    List.map ~f:(fun i -> Exp.DirectProd i) is
  | _ -> failwith "failed sum"
;;

let contains_nonranged_var = Exp.int_exp_contains_var
