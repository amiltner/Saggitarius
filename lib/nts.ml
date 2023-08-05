open MyStdLib

type t = Id.t * int list [@@deriving eq, hash, ord, bin_io, sexp, show]

let compare (id1, il1) (id2, il2) =
  let c1 = Id.compare id1 id2 in
  if not (c1 = 0)
  then c1
  else (
    let rec aux l1 l2 =
      match l1, l2 with
      | [], [] -> 0
      | _ :: _, [] -> -1
      | [], _ :: _ -> 1
      | hd1 :: tl1, hd2 :: tl2 ->
        let c2 = IntModule.compare hd1 hd2 in
        if not (c2 = 0) then c2 else aux tl1 tl2
    in
    aux il1 il2)
;;

(*Fancy show function. Ignores indices when none are provided*)
let show ((id, intlist) : t) : string =
  let rec aux remainingIndices =
    match remainingIndices with
    | [] -> "]"
    | [hd] -> string_of_int hd ^ "]"
    | hd :: tl -> string_of_int hd ^ "," ^ aux tl
  in
  match intlist with
  | [] -> Id.to_string id
  | _ -> Id.to_string id ^ "[" ^ aux intlist
;;
