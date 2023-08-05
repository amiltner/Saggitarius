open MyStdLib

type t =
  | PrefBinding of Exp.range_binding * t
  | Singleton of Preferences.t
[@@deriving eq, hash, ord, show]

let rec size (ph : t) : int =
  match ph with
  | PrefBinding (rb, ph) -> 1 + RangeBinding.size rb + size ph
  | Singleton p -> Preferences.size p
;;

let rec replace_infty_with (i : int) (p : t) : t =
  match p with
  | PrefBinding (rb, p) ->
    PrefBinding
      (Exp.range_binding_replace_infty_with i rb, replace_infty_with i p)
  | Singleton p -> Singleton (Preferences.replace_infty_with i p)
;;

let rec replace_range_with
    (i:Id.t)
    (lr:int)
    (hr:int)
    (ph:t)
  : t =
  begin match ph with
    | PrefBinding ((bid,(rb1,ie1,rb2,ie2)),ph) ->
      let ie1 = Exp.int_exp_replace_var_with i lr ie1 in
      let ie2 = Exp.int_exp_replace_var_with i hr ie2 in
      let ph = replace_range_with i lr hr ph in
      PrefBinding ((bid,(rb1,ie1,rb2,ie2)),ph)
    | Singleton p ->
      Singleton (Preferences.replace_range_with i lr hr p)
  end
