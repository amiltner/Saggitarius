open MyStdLib

type t =
  | EmitBinding of Exp.range_binding * t
  | Singleton of Exp.string_exp * BoolExp.t
[@@deriving eq, hash, ord, show]

let rec size (ph : t) : int =
  match ph with
  | EmitBinding (rb, ph) -> 1 + RangeBinding.size rb + size ph
  | Singleton (p,be) -> Exp.size_string_exp p + BoolExp.size be
;;

let rec replace_infty_with (i : int) (p : t) : t =
  match p with
  | EmitBinding (rb, p) ->
    EmitBinding
      (Exp.range_binding_replace_infty_with i rb, replace_infty_with i p)
  | Singleton (p,b) -> Singleton (Exp.string_exp_replace_infty_with i p, BoolExp.replace_infty_with i b)
;;

let rec replace_range_with
    (i:Id.t)
    (lr:int)
    (hr:int)
    (ph:t)
  : t =
  begin match ph with
    | EmitBinding ((bid,(rb1,ie1,rb2,ie2)),ph) ->
      let ie1 = Exp.int_exp_replace_var_with i lr ie1 in
      let ie2 = Exp.int_exp_replace_var_with i hr ie2 in
      let ph = replace_range_with i lr hr ph in
      EmitBinding ((bid,(rb1,ie1,rb2,ie2)),ph)
    | Singleton (p,be) ->
      Singleton (Exp.string_exp_replace_range_with i lr hr p
                ,Exp.bool_exp_replace_range_with i lr hr be)
  end

let rec make_emit_funs
    (env : (Id.t * int) list)
    (d : (Nts.t * int) list)
    (pm : (Nts.t * int list) list)
    (e : t)
  : (int list -> string option) list =
  match e with
  | Singleton (p,b) ->
    [fun nts ->
       if BoolExp.proc env d pm b nts then
         (Some (Exp.eval_string_exp env p))
       else
         None]
  | EmitBinding ((i, r), p) ->
    let i1, i2 = Exp.eval_range env r in
    List.fold
      ~f:(fun acc v ->
          let env = (i, v) :: env in
          let res = make_emit_funs env d pm p in
          res @ acc)
      ~init:[]
      (range i1 i2)
;;

let full_emit_funs
    (env : (Id.t * int) list)
    (d : (Nts.t * int) list)
    (pm : (Nts.t * int list) list)
    (es : t list)
  : int list -> string list =
  let funs =
    List.concat_map
      ~f:(fun e -> make_emit_funs env d pm e)
      es
  in
  (fun ntss ->
     List.filter_map
       ~f:(fun v -> v ntss)
       funs)
