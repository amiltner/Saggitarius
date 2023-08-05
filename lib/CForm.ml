open MyStdLib

type t =
  | And of t * t
  | Or of t * t
  | Implies of t * t
  | Iff of t * t
  | Not of t
  | Pred of Id.t
  | True
[@@deriving eq, hash, ord, show, variants]

module IDM = DictOf (Id) (IntModule)

let to_constraint (idm : (Id.t * int) list) (c : t) : Constraint.t =
  let idm =
    List.fold
      ~f:(fun idm (k, v) ->
        IDM.insert_or_combine
          ~combiner:(fun _ _ -> failwith "duplicate constraint name")
          idm
          k
          v)
      ~init:IDM.empty
      idm
  in
  let rec to_constraint (c : t) : Constraint.t =
    match c with
    | And (c1, c2) -> Constraint.and_ [to_constraint c1; to_constraint c2]
    | Or (c1, c2) -> Constraint.or_ [to_constraint c1; to_constraint c2]
    | Implies (c1, c2) ->
      Constraint.implies (to_constraint c1) (to_constraint c2)
    | Iff (c1, c2) -> Constraint.iff (to_constraint c1) (to_constraint c2)
    | Not c -> Constraint.not (to_constraint c)
    | Pred i -> Constraint.prodid (IDM.lookup_exn idm i)
    | True -> Constraint.And []
  in
  to_constraint c
;;
