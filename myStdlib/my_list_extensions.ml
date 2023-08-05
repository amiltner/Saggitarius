open Core
open Util
open Algebra

let sorting ~(cmp : 'a comparer) (l : 'a list) : Permutation.t =
  let l_with_is = List.mapi ~f:(fun i x -> x, i) l in
  let sorted_l_with_is =
    List.sort ~compare:(fun (x1, _) (x2, _) -> cmp x1 x2) l_with_is
  in
  Permutation.create (List.map ~f:snd sorted_l_with_is)
;;

let sorting_and_sort ~(cmp : 'a comparer) (l : 'a list)
    : Permutation.t * 'a list
  =
  let l_with_is = List.mapi ~f:(fun i x -> x, i) l in
  let sorted_l_with_is =
    List.sort ~compare:(fun (x1, _) (x2, _) -> cmp x1 x2) l_with_is
  in
  ( Permutation.create (List.map ~f:snd sorted_l_with_is)
  , List.map ~f:fst sorted_l_with_is )
;;

let rec zip3 (l1 : 'a list) (l2 : 'b list) (l3 : 'c list)
    : ('a * 'b * 'c) list option
  =
  match l1, l2, l3 with
  | h1 :: t1, h2 :: t2, h3 :: t3 ->
    Option.map ~f:(fun t -> (h1, h2, h3) :: t) (zip3 t1 t2 t3)
  | [], [], [] -> Some []
  | _ -> None
;;

let zip3_exn (l1 : 'a list) (l2 : 'b list) (l3 : 'c list) : ('a * 'b * 'c) list =
  Option.value_exn (zip3 l1 l2 l3)
;;

let rec sublist_on_sorted ~(cmp : 'a -> 'a -> int) (l1 : 'a list) (l2 : 'a list)
    : bool
  =
  match l1, l2 with
  | [], _ -> true
  | h1 :: t1, h2 :: t2 ->
    let comparison = cmp h1 h2 in
    if is_equal comparison
    then sublist_on_sorted ~cmp t1 t2
    else if is_lt comparison
    then false
    else sublist_on_sorted ~cmp l1 t2
  | _ -> false
;;
