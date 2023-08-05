open MyStdLib

let check_membership (pr : ProvRecorder.t) (rs : RuleSet.t) : bool =
  let rl = RuleSet.as_list rs in
  let il = List.map ~f:(fun x -> x.idx) rl in
  let intS = IntSet.from_list il in
  let rec check_membership (pr : ProvRecorder.t) : bool =
    match pr with
    | Or tl -> List.exists ~f:(fun x -> check_membership x) tl
    | And tl -> List.for_all ~f:(fun x -> check_membership x) tl
    | Rule i -> IntSet.member intS i
  in
  check_membership pr
;;
