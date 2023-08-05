open MyStdLib

(**** RuleSet Module ****)
include SetOf (NumberedId)

let rec show_helper l =
  match l with
  | [] -> ""
  | hd :: tl -> NumberedId.show hd ^ ";" ^ show_helper tl
;;

let show (x : t) : string =
  let temp = show_helper (as_list x) in
  "[" ^ temp ^ "]"
;;

let pp (fmt : Format.formatter) (x : t) = Format.pp_print_string fmt (show x)
