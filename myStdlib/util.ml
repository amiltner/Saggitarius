open Core

let default_compare = Stdlib.compare
let compare _ _ = failwith "don't be so lazy"

let random_char () =
  let random_int = Random.int 256 in
  Char.of_int_exn random_int
;;

let ( % ) (f : 'b -> 'c) (g : 'a -> 'b) : 'a -> 'c = Fn.compose f g

let ( %% ) (f : 'c -> 'd) (g : 'a -> 'b -> 'c) : 'a -> 'b -> 'd =
 fun a b -> f (g a b)
;;

let ( @$ ) (f : 'a -> 'b) (x : 'a) : 'b = f x
let curry (f : 'a * 'b -> 'c) (x : 'a) (y : 'b) : 'c = f (x, y)
let uncurry (f : 'a -> 'b -> 'c) ((x, y) : 'a * 'b) : 'c = f x y
let curry3 (f : 'a * 'b * 'c -> 'd) (x : 'a) (y : 'b) (z : 'c) : 'd = f (x, y, z)

let curry4 (f : 'a * 'b * 'c * 'd -> 'e) (w : 'a) (x : 'b) (y : 'c) (z : 'd)
    : 'e
  =
  f (w, x, y, z)
;;

let uncurry3 (f : 'a -> 'b -> 'c -> 'd) ((x, y, z) : 'a * 'b * 'c) : 'd =
  f x y z
;;

type 'a thunk = unit -> 'a
type 'a continuation = 'a -> 'a
type 'a pper = Format.formatter -> 'a -> unit
type 'a shower = 'a -> string
type 'a hash_folder = Base__Hash.state -> 'a -> Base__Hash.state
type 'a hasher = 'a -> int
type distance = float
type 'a metric = 'a -> 'a -> float
type 'a unfixed = 'a -> 'a

let rec fix (f : 'a unfixed) : 'a = fix f
let fst_trip ((x, _, _) : 'a * 'b * 'c) : 'a = x
let snd_trip ((_, y, _) : 'a * 'b * 'c) : 'b = y
let trd_trip ((_, _, z) : 'a * 'b * 'c) : 'c = z
let fst_quad ((x, _, _, _) : 'a * 'b * 'c * 'd) : 'a = x
let func_of (type a b) (x : a) : b -> a = fun _ -> x
let thunk_of (x : 'a) : 'a thunk = fun _ -> x

type comparison = int [@@deriving ord, show, hash]

let is_equal (c : comparison) : bool = c = 0
let is_lt (c : comparison) : bool = c < 0
let is_gt (c : comparison) : bool = c > 0

type 'a comparer = 'a -> 'a -> int
type 'a equality_check = 'a -> 'a -> bool

type matchable_comparison =
  | EQ
  | LT
  | GT
[@@deriving ord, show, hash]

type partial_order_comparison =
  | PO_EQ
  | PO_LT
  | PO_GT
  | PO_INCOMPARABLE

let compare_list ~(cmp : 'a comparer) : 'a list comparer = compare_list cmp

let compare_list_as_multisets ~(cmp : 'a comparer) (l1 : 'a list) (l2 : 'a list)
    : int
  =
  let sorted_l1 = List.sort ~compare:cmp l1 in
  let sorted_l2 = List.sort ~compare:cmp l2 in
  compare_list ~cmp sorted_l1 sorted_l2
;;

let rec is_sublist ~(cmp : 'a comparer) (l1 : 'a list) (l2 : 'a list) : bool =
  match l1, l2 with
  | [], _ -> true
  | _ :: _, [] -> false
  | h1 :: t1, h2 :: t2 ->
    if is_equal (cmp h1 h2)
    then is_sublist ~cmp t1 t2
    else is_sublist ~cmp l1 t2
;;

let is_submultiset ~(cmp : 'a comparer) (l1 : 'a list) (l2 : 'a list) : bool =
  let sorted_l1 = List.sort ~compare:cmp l1 in
  let sorted_l2 = List.sort ~compare:cmp l2 in
  is_sublist ~cmp sorted_l1 sorted_l2
;;

let make_matchable (n : comparison) : matchable_comparison =
  if n = 0 then EQ else if n < 0 then LT else GT
;;

let make_comparison (c : matchable_comparison) : comparison =
  match c with
  | EQ -> 0
  | LT -> -1
  | GT -> 1
;;

let comparison_to_equality (c : comparison) : bool = c = 0

let comparer_to_equality_check (c : 'a comparer) : 'a equality_check =
 fun x y -> comparison_to_equality (c x y)
;;

let compare_to_equals (f : 'a comparer) (x : 'a) (y : 'a) : bool =
  comparison_to_equality (f x y)
;;

module type Data = sig
  type t

  val show : t shower
  val pp : t pper
  val compare : t comparer
  val hash : t hasher
  val hash_fold_t : t hash_folder
  val equal : t equality_check
end

module type UIDData = sig
  type t

  val show : t shower
  val pp : t pper
  val compare : t comparer
  val hash : t hasher
  val hash_fold_t : t hash_folder
  val equal : t equality_check
  val uid : t -> int
end

module UnitModule = struct
  type t = unit [@@deriving ord, show, hash, eq]

  let uid _ = 0
end

module CharModule = struct
  type t = char [@@deriving ord, show, hash, eq]
end

module IntModule = struct
  type t = int [@@deriving ord, show, hash, eq]

  let distance (x : int) (y : int) : int = Int.abs (x - y)
  let uid = ident
end

module StringModule = struct
  type t = string [@@deriving ord, show, hash, eq]
end

module BoolModule = struct
  type t = bool [@@deriving ord, show, hash, eq]

  let uid (b : bool) : int = if b then 1 else 0
end

module FloatModule = struct
  type t = float [@@deriving ord, show, hash, eq]
end

module RefOf (D : Data) : Data with type t = D.t ref = struct
  type t = D.t ref [@@deriving show]

  let uid (x : t) : int = Obj.magic x
  let compare (x : t) (y : t) : int = Int.compare (uid x) (uid y)
  let equal (x : t) (y : t) : bool = Int.equal (uid x) (uid y)

  let hash_fold_t : 'a ref hash_folder =
   fun hs -> Int.hash_fold_t hs % fun xr -> uid xr
 ;;

  let hash : 'a hasher = Int.hash % fun xr -> uid xr
end

let hash_fold_ref (type a) (_ : a hash_folder) : a ref hash_folder =
 fun hs -> Int.hash_fold_t hs % fun xr -> Obj.magic xr
;;

module OptionOf (D : Data) : Data with type t = D.t option = struct
  type t = D.t option [@@deriving ord, show, hash, eq]
end

let oget (type a) (xo : a option) : a = Option.value_exn xo

module PairOf (D1 : Data) (D2 : Data) : Data with type t = D1.t * D2.t = struct
  type t = D1.t * D2.t [@@deriving ord, show, hash, eq]
end

module TripleOf (D1 : Data) (D2 : Data) (D3 : Data) :
  Data with type t = D1.t * D2.t * D3.t = struct
  type t = D1.t * D2.t * D3.t [@@deriving ord, show, hash, eq]
end

module QuadrupleOf (D1 : Data) (D2 : Data) (D3 : Data) (D4 : Data) :
  Data with type t = D1.t * D2.t * D3.t * D4.t = struct
  type t = D1.t * D2.t * D3.t * D4.t [@@deriving ord, show, hash, eq]
end

module QuintupleOf (D1 : Data) (D2 : Data) (D3 : Data) (D4 : Data) (D5 : Data) :
  Data with type t = D1.t * D2.t * D3.t * D4.t * D5.t = struct
  type t = D1.t * D2.t * D3.t * D4.t * D5.t [@@deriving ord, show, hash, eq]
end

module SextupleOf
    (D1 : Data)
    (D2 : Data)
    (D3 : Data)
    (D4 : Data)
    (D5 : Data)
    (D6 : Data) : Data with type t = D1.t * D2.t * D3.t * D4.t * D5.t * D6.t =
struct
  type t = D1.t * D2.t * D3.t * D4.t * D5.t * D6.t
  [@@deriving ord, show, hash, eq]
end

module SeptupleOf
    (D1 : Data)
    (D2 : Data)
    (D3 : Data)
    (D4 : Data)
    (D5 : Data)
    (D6 : Data)
    (D7 : Data) :
  Data with type t = D1.t * D2.t * D3.t * D4.t * D5.t * D6.t * D7.t = struct
  type t = D1.t * D2.t * D3.t * D4.t * D5.t * D6.t * D7.t
  [@@deriving ord, show, hash, eq]
end

module ListOf (D : Data) : Data with type t = D.t list = struct
  type t = D.t list [@@deriving ord, show, hash, eq]
end

module FloatList = struct
  type t = float list [@@deriving ord, show, hash]

  let sum (l : t) : float = List.fold_left ~f:( +. ) ~init:0.0 l
  let average (l : t) : float = sum l /. Float.of_int (List.length l)
end

module IntList = struct
  type t = int list [@@deriving ord, show, hash, eq]
end

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b
[@@deriving ord, show, hash]

let either_left (e : ('a, 'b) either) : 'a option =
  match e with
  | Left l -> Some l
  | _ -> None
;;

let either_left_exn (e : ('a, 'b) either) : 'a =
  Option.value_exn (either_left e)
;;

let either_right (e : ('a, 'b) either) : 'b option =
  match e with
  | Right r -> Some r
  | _ -> None
;;

let either_right_exn (e : ('a, 'b) either) : 'b =
  Option.value_exn (either_right e)
;;

type 'a except = ('a, string) either

let option_to_either (type a b) ~(f : unit -> b) (xo : a option) : (a, b) either
  =
  match xo with
  | Some x -> Left x
  | None -> Right (f ())
;;

let option_to_except (type a) ~(f : unit -> string) (xo : a option) : a except =
  option_to_either ~f xo
;;

let except_map (type a b) ~(f : a -> b) (xe : a except) : b except =
  match xe with
  | Left x -> Left (f x)
  | Right s -> Right s
;;

let except_bind (type a b) ~(f : a -> b except) (xe : a except) : b except =
  match xe with
  | Left x -> f x
  | Right s -> Right s
;;

let split_by_either (l : ('a, 'b) either list) : 'a list * 'b list =
  let rec split_by_either_internal
      (l : ('a, 'b) either list)
      (c : ('a list * 'b list) continuation)
      : 'a list * 'b list
    =
    match l with
    | [] -> c ([], [])
    | Left x :: t ->
      split_by_either_internal t (fun (ll, lr) -> c (x :: ll, lr))
    | Right x :: t ->
      split_by_either_internal t (fun (ll, lr) -> c (ll, x :: lr))
  in
  split_by_either_internal l ident
;;

let either_map ~(left_f : 'a -> 'c) ~(right_f : 'b -> 'd) (e : ('a, 'b) either)
    : ('c, 'd) either
  =
  match e with
  | Left l -> Left (left_f l)
  | Right r -> Right (right_f r)
;;

let either_join ~(left_f : 'a -> 'c) ~(right_f : 'b -> 'c) (e : ('a, 'b) either)
    : 'c
  =
  match e with
  | Left l -> left_f l
  | Right r -> right_f r
;;

let rec combine_eithers (es : ('a, 'b) either list)
    : ('a list, 'b list) either option
  =
  match es with
  | [] -> Some (Left [])
  | [Left l] -> Some (Left [l])
  | [Right r] -> Some (Right [r])
  | Left l :: es' ->
    begin
      match combine_eithers es' with
      | Some (Left ls) -> Some (Left (l :: ls))
      | _ -> None
    end
  | Right r :: es' ->
    begin
      match combine_eithers es' with
      | Some (Right rs) -> Some (Right (r :: rs))
      | _ -> None
    end
;;

let combine_eithers_exn (es : ('a, 'b) either list) : ('a list, 'b list) either =
  Option.value_exn (combine_eithers es)
;;

type ('a, 'b, 'c) of_three =
  | TLeft of 'a
  | TMiddle of 'b
  | TRight of 'c

let rec fold_until_completion ~(f : 'a -> ('a, 'b) either) (acc : 'a) : 'b =
  match f acc with
  | Left acc' -> fold_until_completion ~f acc'
  | Right answer -> answer
;;

let fold_until_fixpoint ~(is_eq : 'a -> 'a -> bool) (f : 'a -> 'a) : 'a -> 'a =
  fold_until_completion ~f:(fun x ->
      let x' = f x in
      if is_eq x x' then Right x else Left x')
;;

let rec fold_until_right_or_list_end
    ~(f : 'b -> 'a -> ('b, 'c) either)
    ~(init : 'b)
    (l : 'a list)
    : ('b, 'c) either
  =
  match l with
  | [] -> Left init
  | h :: t ->
    let res = f init h in
    begin
      match res with
      | Left continue -> fold_until_right_or_list_end ~f ~init:continue t
      | Right _ -> res
    end
;;

let cartesian_map ~(f : 'a -> 'b -> 'c) (l1 : 'a list) (l2 : 'b list) : 'c list =
  List.fold_right
    ~f:(fun x acc ->
      List.fold_right ~f:(fun y acc2 -> f x y :: acc2) ~init:[] l2 @ acc)
    ~init:[]
    l1
;;

let cartesian_filter_map
    ~(f : 'a -> 'b -> 'c option)
    (l1 : 'a list)
    (l2 : 'b list)
    : 'c list
  =
  List.filter_map ~f:ident (cartesian_map ~f l1 l2)
;;

let cartesian_filter ~(f : 'a -> 'b -> bool) (l1 : 'a list) (l2 : 'b list)
    : ('a * 'b) list
  =
  cartesian_filter_map ~f:(fun x y -> if f x y then Some (x, y) else None) l1 l2
;;

let remove_all_elements (l : 'a list) : ('a * 'a list) list =
  fst
    (List.fold_right
       ~f:(fun x (acc, l) ->
         (x, l) :: List.map ~f:(fun (y, l) -> y, x :: l) acc, x :: l)
       ~init:([], [])
       l)
;;

let range (i : int) (j : int) : int list =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux (j - 1) []
;;

let pair_partition (n : int) : (int * int) list =
  List.map ~f:(fun k -> k, n - k) (range 1 n)
;;

let rec partition (n : int) (buckets : int) : int list list =
  if buckets = 0
  then [[]]
  else if buckets = 1
  then [[n]]
  else (
    let initial_partition = pair_partition n in
    List.concat_map
      ~f:(fun (p1, rest) ->
        let rest_partitioned = partition rest (buckets - 1) in
        List.map ~f:(fun part -> p1 :: part) rest_partitioned)
      initial_partition)
;;

let combinations (type a) (l : a list list) : a list list =
  let rec combinations_internal
      (l : a list list)
      (continuation : a list list -> a list list)
      : a list list
    =
    match l with
    | [] -> continuation [[]]
    | [x] -> continuation (List.rev_map ~f:(fun n -> [n]) x)
    | x :: l ->
      combinations_internal l (fun c ->
          continuation
            (List.fold_left
               ~f:(fun res n ->
                 List.rev_append (List.rev_map ~f:(fun l -> n :: l) c) res)
               ~init:[]
               x))
  in
  combinations_internal l (fun x -> x)
;;

let make_some (x : 'a) : 'a option = Some x

let cons_if_some (xo : 'a option) (l : 'a list) : 'a list =
  match xo with
  | None -> l
  | Some x -> x :: l
;;

let filter_nones (l : 'a option list) : 'a list = List.filter_map ~f:ident l
let option_to_empty_or_singleton (xo : 'a option) : 'a list = cons_if_some xo []

let option_bind ~(f : 'a -> 'b option) (xo : 'a option) : 'b option =
  match xo with
  | None -> None
  | Some x -> f x
;;

let distribute_option (l : 'a option list) : 'a list option =
  List.fold_left
    ~f:(fun acc x ->
      match acc, x with
      | None, _ -> None
      | _, None -> None
      | Some acc', Some x' -> Some (x' :: acc'))
    ~init:(Some [])
    (List.rev l)
;;

let swap_double ((x, y) : 'a * 'b) : 'b * 'a = y, x

let time_action ~(f : unit -> 'a) : float * 'a =
  let t1 = Unix.gettimeofday () in
  let res = f () in
  let t2 = Unix.gettimeofday () in
  t2 -. t1, res
;;

let rec lookup (k : 'a) (l : ('a * 'b) list) : 'b option =
  match l with
  | [] -> None
  | (k', v) :: l -> if k = k' then Some v else lookup k l
;;

let rec split_by_first_satisfying (f : 'a -> bool) (l : 'a list)
    : ('a * 'a list) option
  =
  match l with
  | [] -> None
  | h :: t ->
    if f h
    then Some (h, t)
    else begin
      match split_by_first_satisfying f t with
      | None -> None
      | Some (h', t') -> Some (h', h :: t')
    end
;;

let split_by_first (l : 'a list) : ('a * 'a list) option =
  match l with
  | h :: t -> Some (h, t)
  | [] -> None
;;

let split_by_first_exn (l : 'a list) : 'a * 'a list =
  match l with
  | h :: t -> h, t
  | [] -> failwith "need len at least 1"
;;

let split_by_last_exn (l : 'a list) : 'a list * 'a =
  let h, t = split_by_first_exn (List.rev l) in
  List.rev t, h
;;

let split_by_first_last_exn (l : 'a list) : 'a * 'a list * 'a =
  let h, t = split_by_first_exn l in
  let m, e = split_by_last_exn t in
  h, m, e
;;

let split_by_condition (xs : 'a list) ~(f : 'a -> bool) : 'a list * 'a list =
  let rec split_by_condition_rec
      (xs : 'a list)
      (sats : 'a list)
      (unsats : 'a list)
      : 'a list * 'a list
    =
    match xs with
    | [] -> sats, unsats
    | h :: t ->
      if f h
      then split_by_condition_rec t (h :: sats) unsats
      else split_by_condition_rec t sats (h :: unsats)
  in
  let sats, unsats = split_by_condition_rec xs [] [] in
  List.rev sats, List.rev unsats
;;

let rec all_peels (l : 'a list) : ('a * 'a list) list =
  match l with
  | [] -> []
  | h :: t ->
    let peels =
      List.map ~f:(fun (pe, pl) -> pe, List.cons h pl) (all_peels t)
    in
    (h, t) :: peels
;;

let rec all_peels_split (l : 'a list) : ('a list * 'a * 'a list) list =
  match l with
  | [] -> []
  | h :: t ->
    ([], h, t)
    :: List.map ~f:(fun (t1, v, t2) -> h :: t1, v, t2) (all_peels_split t)
;;

let rec remove_at (l : 'a list) (i : int) : 'a list option =
  match l with
  | [] -> None
  | h :: t ->
    if i = 0
    then Some t
    else Option.map ~f:(fun t -> h :: t) (remove_at t (i - 1))
;;

let remove_at_exn (l : 'a list) (i : int) : 'a list =
  Option.value_exn (remove_at l i)
;;

let split_at_index_exn (l : 'a list) (i : int) : 'a list * 'a list =
  let rec split_at_index_exn_internal
      (l : 'a list)
      (i : int)
      (continuation : 'a list * 'a list -> 'a list * 'a list)
      : 'a list * 'a list
    =
    match l, i with
    | _, 0 -> continuation ([], l)
    | h :: t, _ ->
      split_at_index_exn_internal t (i - 1) (fun (l1, l2) ->
          continuation (h :: l1, l2))
    | _ -> failwith "index out of range"
  in
  if i < 0
  then failwith "invalid index"
  else split_at_index_exn_internal l i (fun x -> x)
;;

let rec remove_at_index_exn (l : 'a list) (i : int) : 'a * 'a list =
  match l with
  | [] -> failwith "bad index"
  | h :: t ->
    if i = 0
    then h, t
    else (
      let x, t = remove_at_index_exn t (i - 1) in
      x, h :: t)
;;

let fold_on_head_exn ~(f : 'a -> 'a -> 'a) (l : 'a list) : 'a =
  let h, t = split_by_first_exn l in
  List.fold_left ~f ~init:h t
;;

let fold_on_head ~(f : 'a -> 'a -> 'a) (l : 'a list) : 'a option =
  match l with
  | [] -> None
  | _ -> Some (fold_on_head_exn ~f l)
;;

let fold_on_head_with_default
    ~(f : 'a -> 'a -> 'a)
    ~default:(d : 'a)
    (l : 'a list)
    : 'a
  =
  match l with
  | [] -> d
  | _ -> fold_on_head_exn ~f l
;;

let weld_lists (f : 'a -> 'a -> 'a) (l1 : 'a list) (l2 : 'a list) : 'a list =
  let head, torso1 = split_by_last_exn l1 in
  let torso2, tail = split_by_first_exn l2 in
  head @ (f torso1 torso2 :: tail)
;;

let duplicate (x : 'a) (n : int) : 'a list =
  let rec duplicate_internal (x : 'a) (n : int) (acc : 'a list) : 'a list =
    if n = 0 then acc else duplicate_internal x (n - 1) (x :: acc)
  in
  duplicate_internal x n []
;;

let bucketize_pairs (num_buckets : int) (data_position_pairs : ('a * int) list)
    : 'a list list
  =
  List.map
    ~f:(fun position ->
      List.filter_map
        ~f:(fun (x, p) -> if position = p then Some x else None)
        data_position_pairs)
    (range 0 num_buckets)
;;

let pair_apply ~(f : 'a -> 'b) ((x, y) : 'a * 'a) : 'b * 'b = f x, f y

let bucketize (f : 'a -> int) (num_buckets : int) (l : 'a list) : 'a list list =
  let data_position_pairs = List.map ~f:(fun x -> x, f x) l in
  bucketize_pairs num_buckets data_position_pairs
;;

let attempt_bucketize (f : 'a -> int option) (num_buckets : int) (l : 'a list)
    : 'a list list option
  =
  let data_position_pairs_option =
    List.map
      ~f:(fun x ->
        match f x with
        | None -> None
        | Some y -> Some (x, y))
      l
  in
  match distribute_option data_position_pairs_option with
  | None -> None
  | Some data_position_pairs ->
    Some
      (List.map
         ~f:(fun position ->
           List.filter_map
             ~f:(fun (x, p) -> if position = p then Some x else None)
             data_position_pairs)
         (range 0 num_buckets))
;;

let transpose_safe_empty_exn (row_count : int) (ls : 'a list list)
    : 'a list list
  =
  if List.length ls = 0 then duplicate [] row_count else List.transpose_exn ls
;;

let is_prime (n : int) : bool =
  let rec loop (k : int) : bool =
    if k * k > n then true else if n mod k = 0 then false else loop (k + 2)
  in
  if n = 2 then true else if n < 2 || n mod 2 = 0 then false else loop 3
;;

let primes_beneath_n (n : int) : int list = List.filter ~f:is_prime (range 0 n)

let primes_between (n : int) (m : int) : int list =
  List.filter ~f:is_prime (range n m)
;;

let rec partitions (n : int) (k : int) : int list list =
  if n = 0 && k = 0
  then [[]]
  else if n <= 0 || k <= 0
  then []
  else if k = 1
  then [[n]]
  else
    List.fold_left
      ~f:(fun res i ->
        List.append res
        @@ List.map ~f:(fun t -> i :: t) (partitions (n - i) (k - 1)))
      ~init:[]
      (List.map ~f:(( + ) 1) (range 0 (n - k + 1)))
;;

let double_partitions (n : int) : (int * int) list =
  let list_split_partitions = partitions n 2 in
  List.map
    ~f:(fun pl ->
      match pl with
      | [f; s] -> f, s
      | _ -> failwith "bug in double_partitions")
    list_split_partitions
;;

let triple_partitions (n : int) : (int * int * int) list =
  let list_split_partitions = partitions n 3 in
  List.map
    ~f:(fun tl ->
      match tl with
      | [f; s; t] -> f, s, t
      | _ -> failwith "bug in triple_partitions")
    list_split_partitions
;;

let rec sort_and_partition ~(cmp : 'a -> 'a -> comparison) (l : 'a list)
    : 'a list list
  =
  let rec merge_sorted_partitions (l1 : 'a list list) (l2 : 'a list list)
      : 'a list list
    =
    match l1, l2 with
    | h1 :: t1, h2 :: t2 ->
      let rep1 = List.hd_exn h1 in
      let rep2 = List.hd_exn h2 in
      let comparison = cmp rep1 rep2 in
      if comparison = 0
      then (h1 @ h2) :: merge_sorted_partitions t1 t2
      else if comparison < 0
      then h1 :: merge_sorted_partitions t1 l2
      else h2 :: merge_sorted_partitions l1 t2
    | _ -> l1 @ l2
  in
  match l with
  | [] -> []
  | [h] -> [[h]]
  | _ ->
    let len = List.length l in
    let l1, l2 = split_at_index_exn l (len / 2) in
    let sorted_partitioned_l1 = sort_and_partition ~cmp l1 in
    let sorted_partitioned_l2 = sort_and_partition ~cmp l2 in
    merge_sorted_partitions sorted_partitioned_l1 sorted_partitioned_l2
;;

let sort_and_partition_with_indices (f : 'a -> 'a -> comparison) (l : 'a list)
    : ('a * int) list list
  =
  (*let rec merge_sorted_partitions (l1:('a * int) list list)
                (l2:('a * int) list list) : ('a * int) list list =
    begin match (l1,l2) with
    | (h1::t1,h2::t2) ->
        let (rep1,_) = List.hd_exn h1 in
        let (rep2,_) = List.hd_exn h2 in
        let comparison = f rep1 rep2 in
        begin match comparison with
        | EQ -> ((h1@h2)::(merge_sorted_partitions t1 t2))
        | LT -> (h1::(merge_sorted_partitions t1 l2))
        | GT -> (h2::(merge_sorted_partitions l1 t2))
        end
    | _ -> l1 @ l2
    end in
  let rec sort_and_partition_with_indices_internal (l:('a * int) list)
                      : ('a * int) list list =*)
  let rec merge_grouped_things
      (remaining : ('a * int) list)
      (currentacc : ('a * int) list)
      (accacc : ('a * int) list list)
      : ('a * int) list list
    =
    match remaining with
    | [] -> currentacc :: accacc
    | (h, i) :: t ->
      let currenthd = fst (List.hd_exn currentacc) in
      let cmp = f h currenthd in
      if cmp = 0
      then merge_grouped_things t ((h, i) :: currentacc) accacc
      else merge_grouped_things t [h, i] (currentacc :: accacc)
  in
  let sorted =
    List.sort
      ~compare:(fun (x, _) (y, _) -> f x y)
      (List.mapi ~f:(fun i x -> x, i) l)
  in
  match sorted with
  | [] -> []
  | h :: t -> merge_grouped_things t [h] []
;;

(*begin match l with
    | [] -> []
    | [h] -> [[h]]
    | _ ->
        let len = List.length l in
        let (l1, l2) = split_at_index_exn l (len/2) in
        let sorted_partitioned_l1 = sort_and_partition_with_indices_internal l1 in
        let sorted_partitioned_l2 = sort_and_partition_with_indices_internal l2 in
        merge_sorted_partitions sorted_partitioned_l1 sorted_partitioned_l2
    end in
  sort_and_partition_with_indices_internal
    (List.mapi ~f:(fun i x -> (x,i)) l)*)

let ordered_partition_order
    (f : 'a -> 'a -> comparison)
    (l1 : 'a list)
    (l2 : 'a list)
    : comparison
  =
  let p1 = sort_and_partition ~cmp:f l1 in
  let p2 = sort_and_partition ~cmp:f l2 in
  let cmp = compare_int (List.length p1) (List.length p2) in
  if cmp = 0
  then
    List.fold_left
      ~f:(fun acc (l1', l2') ->
        if is_equal acc then compare_list ~cmp:f l1' l2' else acc)
      ~init:0
      (List.zip_exn p1 p2)
  else cmp
;;

let option_compare
    (value_compare : 'a -> 'a -> comparison)
    (xo : 'a option)
    (yo : 'a option)
    : comparison
  =
  match xo, yo with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x, Some y -> value_compare x y
;;

let either_compare
    (left_compare : 'a -> 'a -> comparison)
    (right_compare : 'a -> 'a -> comparison)
    (xe : ('a, 'b) either)
    (ye : ('a, 'b) either)
    : comparison
  =
  match xe, ye with
  | Left xl, Left yl -> left_compare xl yl
  | Left _, _ -> -1
  | Right xr, Right yr -> right_compare xr yr
  | Right _, _ -> 1
;;

let pair_compare
    (fst_compare : 'a comparer)
    (snd_compare : 'b comparer)
    ((x1, x2) : 'a * 'b)
    ((y1, y2) : 'a * 'b)
    : comparison
  =
  let cmp = fst_compare x1 y1 in
  if is_equal cmp then snd_compare x2 y2 else cmp
;;

let triple_compare
    (fst_compare : 'a -> 'a -> comparison)
    (snd_compare : 'b -> 'b -> comparison)
    (trd_compare : 'c -> 'c -> comparison)
    ((x1, x2, x3) : 'a * 'b * 'c)
    ((y1, y2, y3) : 'a * 'b * 'c)
    : comparison
  =
  let cmp = fst_compare x1 y1 in
  if is_equal cmp
  then pair_compare snd_compare trd_compare (x2, x3) (y2, y3)
  else cmp
;;

let quad_compare
    (fst_compare : 'a -> 'a -> comparison)
    (snd_compare : 'b -> 'b -> comparison)
    (trd_compare : 'c -> 'c -> comparison)
    (rth_compare : 'd -> 'd -> comparison)
    ((x1, x2, x3, x4) : 'a * 'b * 'c * 'd)
    ((y1, y2, y3, y4) : 'a * 'b * 'c * 'd)
    : comparison
  =
  let cmp = fst_compare x1 y1 in
  if is_equal cmp
  then
    triple_compare snd_compare trd_compare rth_compare (x2, x3, x4) (y2, y3, y4)
  else cmp
;;

let quint_compare
    (fst_compare : 'a -> 'a -> comparison)
    (snd_compare : 'b -> 'b -> comparison)
    (trd_compare : 'c -> 'c -> comparison)
    (rth_compare : 'd -> 'd -> comparison)
    (fth_compare : 'e -> 'e -> comparison)
    ((x1, x2, x3, x4, x5) : 'a * 'b * 'c * 'd * 'e)
    ((y1, y2, y3, y4, y5) : 'a * 'b * 'c * 'd * 'e)
    : comparison
  =
  let cmp = fst_compare x1 y1 in
  if is_equal cmp
  then
    quad_compare
      snd_compare
      trd_compare
      rth_compare
      fth_compare
      (x2, x3, x4, x5)
      (y2, y3, y4, y5)
  else cmp
;;

let sext_compare
    (fst_compare : 'a -> 'a -> comparison)
    (snd_compare : 'b -> 'b -> comparison)
    (trd_compare : 'c -> 'c -> comparison)
    (rth_compare : 'd -> 'd -> comparison)
    (fth_compare : 'e -> 'e -> comparison)
    (sth_compare : 'f -> 'f -> comparison)
    ((x1, x2, x3, x4, x5, x6) : 'a * 'b * 'c * 'd * 'e * 'f)
    ((y1, y2, y3, y4, y5, y6) : 'a * 'b * 'c * 'd * 'e * 'f)
    : comparison
  =
  let cmp = fst_compare x1 y1 in
  if is_equal cmp
  then
    quint_compare
      snd_compare
      trd_compare
      rth_compare
      fth_compare
      sth_compare
      (x2, x3, x4, x5, x6)
      (y2, y3, y4, y5, y6)
  else cmp
;;

let partition_dictionary_order (f : 'a comparer) : 'a list list comparer =
  compare_list ~cmp:(fun x y -> f (List.hd_exn x) (List.hd_exn y))
;;

let ordered_partition_dictionary_order (f : 'a -> 'a -> comparison)
    : ('a * int) list list comparer
  =
  compare_list ~cmp:(fun x y ->
      let cmp = Int.compare (List.length x) (List.length y) in
      if is_equal cmp
      then f (fst (List.hd_exn x)) (fst (List.hd_exn y))
      else cmp)
;;

let intersect_map_lose_order_and_dupes
    (type a b)
    ~(f : a -> a -> b)
    ~(cmp : a -> a -> comparison)
    (l1 : a list)
    (l2 : a list)
    : b list
  =
  let rec intersect_ordered (l1 : a list) (l2 : a list) : b list =
    match l1, l2 with
    | h1 :: t1, h2 :: t2 ->
      let cmp = cmp h1 h2 in
      if is_equal cmp
      then f h1 h2 :: intersect_ordered t1 t2
      else if is_lt cmp
      then intersect_ordered t1 l2
      else intersect_ordered l1 t2
    | [], _ -> []
    | _, [] -> []
  in
  let ordered_l1 = List.dedup_and_sort ~compare:cmp l1 in
  let ordered_l2 = List.dedup_and_sort ~compare:cmp l2 in
  intersect_ordered ordered_l1 ordered_l2
;;

let intersect_lose_order_and_dupes
    (cmp : 'a -> 'a -> comparison)
    (l1 : 'a list)
    (l2 : 'a list)
    : 'a list
  =
  intersect_map_lose_order_and_dupes ~f:(fun x _ -> x) ~cmp l1 l2
;;

let minus_keys_lose_order
    (cmp : 'a -> 'a -> comparison)
    (l1 : ('a * 'b) list)
    (l2 : 'a list)
    : ('a * 'b) list
  =
  let rec set_minus_ordered (l1 : ('a * 'b) list) (l2 : 'a list)
      : ('a * 'b) list
    =
    match l1, l2 with
    | (h1, v1) :: t1, h2 :: t2 ->
      let cmp = cmp h1 h2 in
      if is_equal cmp
      then set_minus_ordered t1 l2
      else if is_lt cmp
      then (h1, v1) :: set_minus_ordered t1 l2
      else set_minus_ordered l1 t2
    | [], _ -> []
    | _, [] -> l1
  in
  let ordered_l1 = List.sort ~compare:(fun (k1, _) (k2, _) -> cmp k1 k2) l1 in
  let ordered_l2 = List.dedup_and_sort ~compare:cmp l2 in
  set_minus_ordered ordered_l1 ordered_l2
;;

let set_minus_lose_order
    (cmp : 'a -> 'a -> comparison)
    (l1 : 'a list)
    (l2 : 'a list)
    : 'a list
  =
  let rec set_minus_ordered (l1 : 'a list) (l2 : 'a list) : 'a list =
    match l1, l2 with
    | h1 :: t1, h2 :: t2 ->
      let cmp = cmp h1 h2 in
      if is_equal cmp
      then set_minus_ordered t1 t2
      else if is_lt cmp
      then h1 :: set_minus_ordered t1 l2
      else set_minus_ordered l1 t2
    | [], _ -> []
    | _, [] -> l1
  in
  let ordered_l1 =
    List.dedup_and_sort ~compare:cmp (List.sort ~compare:cmp l1)
  in
  let ordered_l2 =
    List.dedup_and_sort ~compare:cmp (List.sort ~compare:cmp l2)
  in
  set_minus_ordered ordered_l1 ordered_l2
;;

let symmetric_set_minus
    (cmp : 'a -> 'a -> comparison)
    (l1 : 'a list)
    (l2 : 'a list)
    : ('a, 'a) either list
  =
  List.map ~f:(fun x -> Left x) (set_minus_lose_order cmp l1 l2)
  @ List.map ~f:(fun x -> Right x) (set_minus_lose_order cmp l1 l2)
;;

let pairwise_maintain_invariant
    (invariant : 'a -> 'a -> bool)
    (l1 : 'a list)
    (l2 : 'a list)
    : bool
  =
  List.for_all ~f:(fun x -> List.for_all ~f:(invariant x) l2) l1
;;

let project_out_elements (indices : int list) (l : 'a list) : 'a list =
  let rec project_out_elements_internal
      (ois : int list)
      (l : 'a list)
      (current_index : int)
      : 'a list
    =
    match ois, l with
    | [], _ -> l
    | i :: ois', x :: xs ->
      if i = current_index
      then project_out_elements_internal ois' xs (current_index + 1)
      else x :: project_out_elements_internal ois xs (current_index + 1)
    | _ :: _, [] -> failwith "bad elements"
  in
  project_out_elements_internal (List.sort ~compare:Int.compare indices) l 0
;;

let rec zip_nondist (xs : 'a list) (ys : 'b list) : ('a option * 'b option) list
  =
  match xs, ys with
  | x :: xs, y :: ys -> (Some x, Some y) :: zip_nondist xs ys
  | [], _ -> List.map ~f:(fun y -> None, Some y) ys
  | _, [] -> List.map ~f:(fun x -> Some x, None) xs
;;

let rec zip_with
    (xs : 'a list)
    (ys : 'b list)
    (f_match : 'a -> 'b -> 'c)
    (unmatch_left : 'a -> 'c)
    (unmatch_right : 'b -> 'c)
    : 'c list
  =
  match xs, ys with
  | h1 :: t1, h2 :: t2 ->
    f_match h1 h2 :: zip_with t1 t2 f_match unmatch_left unmatch_right
  | _, [] -> List.map ~f:unmatch_left xs
  | [], _ -> List.map ~f:unmatch_right ys
;;

let rec zip3 (xs : 'a list) (ys : 'b list) (zs : 'c list)
    : ('a * 'b * 'c) list option
  =
  match xs, ys, zs with
  | x :: xs, y :: ys, z :: zs ->
    Option.map ~f:(fun xyzs -> (x, y, z) :: xyzs) (zip3 xs ys zs)
  | [], [], [] -> Some []
  | _ -> None
;;

let zip3_exn (xs : 'a list) (ys : 'b list) (zs : 'c list) : ('a * 'b * 'c) list =
  Option.value_exn (zip3 xs ys zs)
;;

let rec assoc_value_mem (value : 'b) (l : ('a * 'b) list) : 'a option =
  match l with
  | (k, v) :: t -> if value = v then Some k else assoc_value_mem value t
  | [] -> None
;;

let rec insert_into_correct_list
    ~(is_eq : 'a -> 'a -> bool)
    (l : ('a * 'b list) list)
    (k : 'a)
    (v : 'b)
    : ('a * 'b list) list
  =
  match l with
  | (k', vlist) :: kvplist ->
    if is_eq k k'
    then (k', v :: vlist) :: kvplist
    else (k', vlist) :: insert_into_correct_list ~is_eq kvplist k v
  | [] -> [k, [v]]
;;

let rec append_into_correct_list
    ((k, v) : 'a * 'b list)
    (l : ('a * 'b list) list)
    : ('a * 'b list) list
  =
  match l with
  | (k', vlist) :: kvplist ->
    if k = k'
    then (k', v @ vlist) :: kvplist
    else (k', vlist) :: append_into_correct_list (k, v) kvplist
  | [] -> failwith "bad lisat"
;;

let group_by_values (l : ('a list * 'b) list) : ('a list * 'b) list =
  let empty_value_list =
    List.dedup_and_sort
      ~compare:default_compare
      (List.map ~f:(fun v -> snd v, []) l)
  in
  let l' =
    List.fold_left
      ~f:(fun acc (k, v) -> append_into_correct_list (v, k) acc)
      ~init:empty_value_list
      l
  in
  List.map ~f:(fun (x, y) -> y, x) l'
;;

let group_by_keys ~(is_eq : 'a -> 'a -> bool) (kvl : ('a * 'b) list)
    : ('a * 'b list) list
  =
  List.fold_left
    ~f:(fun acc (k, v) -> insert_into_correct_list ~is_eq acc k v)
    ~init:[]
    kvl
;;

module Operators = struct
  let ( >?> ) (x : 'a option) (f : 'a -> 'b option) : 'b option =
    match x with
    | None -> None
    | Some v -> f v
  ;;
end

let string_to_char_list (s : string) : char list =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let hash_pair (fst_hash : 'a -> int) (snd_hash : 'b -> int) ((a, b) : 'a * 'b)
    : int
  =
  fst_hash a lxor snd_hash b
;;

let hash_triple
    (fst_hash : 'a -> int)
    (snd_hash : 'b -> int)
    (trd_hash : 'c -> int)
    ((a, b, c) : 'a * 'b * 'c)
    : int
  =
  fst_hash a lxor snd_hash b lxor trd_hash c
;;

let hash_quadruple
    (fst_hash : 'a -> int)
    (snd_hash : 'b -> int)
    (trd_hash : 'c -> int)
    (rth_hash : 'd -> int)
    ((a, b, c, d) : 'a * 'b * 'c * 'd)
    : int
  =
  fst_hash a lxor snd_hash b lxor trd_hash c lxor rth_hash d
;;

let hash_quintuple
    (fst_hash : 'a -> int)
    (snd_hash : 'b -> int)
    (trd_hash : 'c -> int)
    (rth_hash : 'd -> int)
    (fth_hash : 'e -> int)
    ((a, b, c, d, e) : 'a * 'b * 'c * 'd * 'e)
    : int
  =
  fst_hash a lxor snd_hash b lxor trd_hash c lxor rth_hash d lxor fth_hash e
;;

type 'a sequence =
  | SNil
  | SCons of 'a * 'a sequence thunk

let rec app_seq (s1 : 'a sequence) (s2 : 'a sequence) : 'a sequence =
  match s1 with
  | SNil -> s2
  | SCons (x, s1't) -> SCons (x, fun () -> app_seq (s1't ()) s2)
;;

module type MetricSpaceData = sig
  type t

  val show : t shower
  val pp : t pper
  val compare : t comparer
  val hash : t hasher
  val hash_fold_t : t hash_folder
  val dist : t metric
end

module Math = struct
  let rec factorial (n : int) : float =
    if n = 0 then 1.0 else Float.of_int n *. factorial (n - 1)
  ;;

  include Posix_math
end

module Id = struct
  type t = Id of string [@@deriving eq, hash, ord, show, sexp, bin_io]

  let mk_prime (Id x : t) : t = Id (x ^ "'")
  let create (s : string) : t = Id s
  let from_int (i : int) : t = Id (string_of_int i)
  let destruct (Id x : t) : string = x
  let to_string (Id s) : string = s
end

let rec do_until_completion ~(init : int) ~(step : int) (f : int -> 'a option)
    : 'a
  =
  match f init with
  | Some v -> v
  | None -> do_until_completion ~init:(init + step) ~step f
;;

let sum_int_list l = List.fold ~f:( + ) ~init:0 l

let fst4 (x,_,_,_) = x
let snd4 (_,x,_,_) = x
let trd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x

let fst5 (x,_,_,_,_) = x
let snd5 (_,x,_,_,_) = x
let trd5 (_,_,x,_,_) = x
let for5 (_,_,_,x,_) = x
let fth5 (_,_,_,_,x) = x

let rec random_subset (i:int) (l:'a list) : 'a list =
  if i = 0 then []
  else
    let index = Random.int (List.length l) in
    let (ll,lr) = split_at_index_exn l index in
    let (v,lr) = split_by_first_exn lr in
    v::(random_subset (i-1) l)

let random_element (l:'a list) : 'a * 'a list =
  let index = Random.int (List.length l) in
  let (ll,lr) = split_at_index_exn l index in
  let (v,lr) = split_by_first_exn lr in
  (v,ll@lr)

