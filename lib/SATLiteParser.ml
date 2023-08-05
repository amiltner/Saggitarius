open MyStdLib

let parse (pr : ProvRecorder.t) (rs : RuleSet.t) : RuleSet.t option =
  let rl = RuleSet.as_list rs in
  let il = List.map ~f:(fun x -> x.idx) rl in
  let intS = IntSet.from_list il in
  (*aux passes back up a parse of everythin below.*)
  let rec aux (p : ProvRecorder.t) : IntSet.t option =
    match p with
    | Or tl ->
      let rec checkNext remaining =
        match remaining with
        | [] -> None
        | branch :: tl2 ->
          (match aux branch with
          | None -> checkNext tl2
          | Some branchParse -> Some branchParse)
      in
      checkNext tl
    | And tl ->
      List.fold
        ~f:(fun acco x ->
          match acco with
          | None -> None
          | Some acc ->
            begin
              match aux x with
              | None -> None
              | Some p -> Some (IntSet.union acc p)
            end)
        ~init:(Some IntSet.empty)
        tl
    | Rule i -> if IntSet.member intS i then Some (IntSet.singleton i) else None
  in
  let intRes = aux pr in
  match intRes with
  | None -> None
  | Some intSParse ->
    Some (RuleSet.filter ~f:(fun rule -> IntSet.member intSParse rule.idx) rs)
;;
