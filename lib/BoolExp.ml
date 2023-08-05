open MyStdLib

type t = Exp.bool_exp [@@deriving eq, hash, ord, show]

let replace_infty_with = Exp.bool_exp_replace_infty_with
let eval = Exp.eval_bool_exp
let size = Exp.size_bool_exp

let rec to_constraint
    (env : (Id.t * int) list)
    (d : (Nts.t * int) list)
    (pm : (Nts.t * int list) list)
    (b : t)
    : Constraint.t
  =
  match b with
  | IndexedId iid ->
    let io = Exp.indexed_id_to_int env d iid in
    begin
      match io with
      | None -> Constraint.Or []
      | Some i -> Constraint.ProdId i
    end
  | Not be -> Constraint.Not (to_constraint env d pm be)
  | Disjunct (be1, be2) ->
    Constraint.Or [to_constraint env d pm be1; to_constraint env d pm be2]
  | Implies (be1, be2) ->
    Constraint.Implies (to_constraint env d pm be1, to_constraint env d pm be2)
  | Iff (be1, be2) ->
    Constraint.Iff (to_constraint env d pm be1, to_constraint env d pm be2)
  | Conjunct (be1, be2) ->
    Constraint.And [to_constraint env d pm be1; to_constraint env d pm be2]
  | True -> Constraint.Or []
  | False -> Constraint.And []
  | OrOver ((i, r), be) ->
    let i1, i2 = Exp.eval_range env r in
    let vs = range i1 i2 in
    Constraint.Or
      (List.map ~f:(fun v -> to_constraint ((i, v) :: env) d pm be) vs)
  | AndOver ((i, r), be) ->
    let i1, i2 = Exp.eval_range env r in
    let vs = range i1 i2 in
    Constraint.And
      (List.map ~f:(fun v -> to_constraint ((i, v) :: env) d pm be) vs)
  | LT (ie1, ie2) ->
    let ie2 = IntExp.eval env ie2 in
    let bs = IntExp.to_sum_exn env d pm ie1 in
    Constraint.LessThan (List.map ~f:(to_constraint env d pm) bs, ie2)
  | GT (ie1, ie2) ->
    let ie2 = IntExp.eval env ie2 in
    let bs = IntExp.to_sum_exn env d pm ie1 in
    Constraint.GreaterThan (List.map ~f:(to_constraint env d pm) bs, ie2)
  | EQ (ie1, ie2) ->
    let ie2 = IntExp.eval env ie2 in
    let bs = IntExp.to_sum_exn env d pm ie1 in
    Constraint.Eq (List.map ~f:(to_constraint env d pm) bs, ie2)
  | DirectProd i -> Constraint.ProdId i
;;

let rec proc
    (env : (Id.t * int) list)
    (d : (Nts.t * int) list)
    (pm : (Nts.t * int list) list)
    (be : t)
    (resolved:int list)
  : bool =
  match be with
  | IndexedId iid ->
    let io = Exp.indexed_id_to_int env d iid in
    begin
      match io with
      | None -> false
      | Some i -> List.mem ~equal:Int.equal resolved i
    end
  | Not be -> not (proc env d pm be resolved)
  | Disjunct (be1, be2) ->
    proc env d pm be1 resolved || proc env d pm be2 resolved
  | Implies (be1, be2) ->
    not (proc env d pm be1 resolved) || (proc env d pm be2 resolved)
  | Iff (be1, be2) ->
    (proc env d pm (Implies (be1,be2)) resolved) && (proc env d pm (Implies (be2,be1)) resolved)
  | Conjunct (be1, be2) ->
    (proc env d pm be1 resolved) && (proc env d pm be2 resolved)
  | True -> true
  | False -> false
  | OrOver ((i, r), be) ->
    let i1, i2 = Exp.eval_range env r in
    let vs = range i1 i2 in
    List.exists
      ~f:(fun v -> proc ((i, v) :: env) d pm be resolved)
      vs
  | AndOver ((i, r), be) ->
    let i1, i2 = Exp.eval_range env r in
    let vs = range i1 i2 in
    List.for_all
      ~f:(fun v -> proc ((i, v) :: env) d pm be resolved)
      vs
  | LT (ie1, ie2) ->
    failwith "not permitting"
  | GT (ie1, ie2) ->
    failwith "not permitting"
  | EQ (ie1, ie2) ->
    failwith "not permitting"
  | DirectProd i ->
    failwith "not permitting"
;;
