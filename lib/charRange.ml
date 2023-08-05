open MyStdLib

module CharRange = struct
  type t = int * int [@@deriving bin_io, eq, hash, ord, sexp, show]

  let create_from_ints a b : t = a, b
  let create_from_single (a : char) : t = Char.to_int a, Char.to_int a
  let create_from_chars (a : char) (b : char) : t = Char.to_int a, Char.to_int b

  (* Ranges are both sides inclusive.
   * I don't care about empty ranges since Symbol.ml
   * will have terminal of range list and that can be empty.*)
  let is_member (c : char) ((a, b) : t) : bool =
    let n = Char.to_int c in
    a <= n && b >= n
  ;;

  let show_int ((a, b) : t) : string =
    let s1 = string_of_int a in
    let s2 = string_of_int b in
    if a = b then "[" ^ s1 ^ "]" else "[" ^ s1 ^ ".." ^ s2 ^ "]"
  ;;

  let show ((a, b) : t) : string =
    let s1 = Char.escaped (Stdlib.Char.chr a) in
    let s2 = Char.escaped (Stdlib.Char.chr b) in
    if a = b then "[" ^ s1 ^ "]" else "[" ^ s1 ^ ".." ^ s2 ^ "]"
  ;;

  let size ((i1, i2) : t) : int = if i1 = i2 then 1 else 2
end

(*Char Range List*)
module CRList = struct
  type t = CharRange.t list [@@deriving bin_io, eq, hash, ord, sexp, show]

  let size crl =
    let crs = List.map ~f:CharRange.size crl in
    sum_int_list crs
  ;;

  let is_singleton (x : t) : bool =
    match x with
    | [x] -> CharRange.size x = 1
    | _ -> false
  ;;

  (*Constructors*)
  let empty : t = []

  let create_from_chars (a : Char.t) (b : Char.t) : t =
    [CharRange.create_from_chars a b]
  ;;

  (*add single element*)
  let add (cr : CharRange.t) (crl : t) : t = cr :: crl

  let compress (x : t) : t =
    let s = List.sort x (fun (a1, _) (a2, _) -> IntModule.compare a1 a2) in
    let rec helper (remaining : t) (running : t) (hyp : (int * int) option) : t =
      match hyp with
      | None ->
        (match remaining with
        | [] -> running
        | (a, b) :: tl -> helper tl running (Some (a, b)))
      | Some (i, j) ->
        (match remaining with
        | [] -> (i, j) :: running
        | (ni, nj) :: tl ->
          if ni <= j + 1
          then (
            let new_hyp = Some (i, max j nj) in
            helper tl running new_hyp)
          else (
            let new_run = (i, j) :: running in
            let new_hyp = Some (ni, nj) in
            helper tl new_run new_hyp))
    in
    let compressed = helper s [] None in
    let size_comparison (a1, b1) (a2, b2) =
      let s1 = b1 - a1 in
      let s2 = b2 - a2 in
      IntModule.compare s2 s1
      (*sneaky way to sort by largest size first*)
    in
    List.sort compressed size_comparison
  ;;

  let smart_add (cr : CharRange.t) (crl : t) : t = compress (cr :: crl)

  let is_member (c : Char.t) (crl : t) : bool =
    List.exists crl ~f:(fun cr -> CharRange.is_member c cr)
  ;;

  (*Printing*)
  let show (x : t) : string =
    let rec aux (left : t) (running : string) =
      match left with
      | [] -> running ^ ")"
      | [hd] -> aux [] (running ^ CharRange.show hd)
      | hd :: tl -> aux tl (running ^ CharRange.show hd ^ ",")
    in
    aux x "("
  ;;

  let show_int (x : t) : string =
    let rec aux (left : t) (running : string) =
      match left with
      | [] -> running ^ ")"
      | [hd] -> aux [] (running ^ CharRange.show_int hd)
      | hd :: tl -> aux tl (running ^ CharRange.show_int hd ^ ",")
    in
    aux x "("
  ;;
end
