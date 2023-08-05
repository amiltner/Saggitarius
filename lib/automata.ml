open MyStdLib
open CharRange

module CharMap = struct
  include Map.Make (Char)
  include Provide_bin_io (Char)
  include Provide_hash (Char)
end

let current_id = ref 0

type tag =
  | Red
  | Blue
[@@deriving eq, hash, ord, show, sexp, bin_io]

let initial_tag = Red

let switch_tag (t : tag) : tag =
  match t with
  | Red -> Blue
  | Blue -> Red
;;

type t =
  { uid : int
  ; node : t_node
  ; mutable derivative : t CharMap.t
  ; mutable parse : NumberedId.t list option
  ; mutable p_calced : bool
  ; mutable p_tag : tag
  ; mutable compacted : t option
  }

and t_node =
  | Union of t Lazy.t list
  | Concat of t Lazy.t list
  | Char of Char.t
  | Range of CRList.t
  | Reduction of (NumberedId.t list -> NumberedId.t list) * t

let compare (x1 : t) (x2 : t) : int = Int.compare x1.uid x2.uid
let equal (x1 : t) (x2 : t) : bool = Int.equal x1.uid x2.uid
let uid (s : t) = s.uid

let create (n : t_node) : t =
  let uid = !current_id in
  current_id := !current_id + 1;
  { uid
  ; node = n
  ; derivative = CharMap.empty
  ; parse = None
  ; p_calced = false
  ; p_tag = initial_tag
  ; compacted = None
  }
;;

let node (x : t) : t_node = x.node

let is_epsilon (x : t) : bool =
  match node x with
  | Concat [] -> true
  | _ -> false
;;

let is_empty (x : t) : bool =
  match node x with
  | Union [] -> true
  | _ -> false
;;

let apply_to_union (type a) ~(f : t list -> a) (x : t) : a option =
  match node x with
  | Union xs -> Some (f (List.map ~f:Lazy.force xs))
  | _ -> None
;;

let destruct_union = apply_to_union ~f:ident

let apply_to_concat (type a) ~(f : t list -> a) (x : t) : a option =
  match node x with
  | Concat xs -> Some (f (List.map ~f:Lazy.force xs))
  | _ -> None
;;

let destruct_concat = apply_to_concat ~f:ident
let empty = create (Union [])
let epsilon = create (Concat [])
let char c = create (Char c)

let singleton s =
  create
    (Concat (List.rev_map ~f:(Lazy.from_val % char) (String.to_list_rev s)))
;;

let range crl = create (Range crl)
let union (xs : t Lazy.t list) : t = create (Union xs)
let concat (xs : t Lazy.t list) : t = create (Concat xs)

let reduction (f : NumberedId.t list -> NumberedId.t list) (x : t) : t =
  create (Reduction (f, x))
;;

let from_grammar_production (g : Grammar.t) (p : Grammar.Production.t) : t =
  let module GM = struct
    include Map.Make (Nts)
    include Provide_bin_io (Nts)
    include Provide_hash (Nts)
  end
  in
  let map = ref GM.empty in
  let rec from_production (p : Grammar.Production.t) : t =
    match p with
    | Nonterminal i ->
      begin
        match GM.find !map i with
        | None ->
          let inner = Grammar.get_productions g i in
          let ans = from_production inner in
          map := GM.add_exn ~key:i ~data:ans !map;
          ans
        | Some v -> v
      end
    | Terminal s -> singleton s
    | Concat (p1, p2) ->
      let a1 = Lazy.from_fun (fun () -> from_production p1) in
      let a2 = Lazy.from_fun (fun () -> from_production p2) in
      concat [a1; a2]
    | Empty -> empty
    | Or (p1, p2) ->
      let a1 = Lazy.from_fun (fun () -> from_production p1) in
      let a2 = Lazy.from_fun (fun () -> from_production p2) in
      union [a1; a2]
    | Labelled (i, p) -> reduction (fun xs -> i :: xs) (from_production p)
    | Range crl -> range crl
  in
  from_production p
;;

let from_grammar (g : Grammar.t) : t =
  from_grammar_production g (Grammar.get_initial_production g)
;;

let materialize_nullables (x : t) : unit =
  let changed = ref true in
  let perform_if_p_tag (x : t) (next_tag : tag) (f : unit -> unit) : unit =
    if equal_tag x.p_tag next_tag
    then ()
    else (
      x.p_tag <- next_tag;
      f ())
  in
  let rec update_singlestep_nullables (next_tag : tag) (x : t) : unit =
    perform_if_p_tag x next_tag (fun () ->
        match node x with
        | Union xs ->
          let xs = List.map ~f:Lazy.force_val xs in
          if Option.is_none x.parse
          then begin
            match List.find_map ~f:(fun x -> x.parse) xs with
            | Some bs as bso ->
              changed := true;
              x.parse <- bso
            | None -> ()
          end;
          List.iter ~f:(update_singlestep_nullables next_tag) xs
        | Concat xs ->
          let xs = List.map ~f:Lazy.force_val xs in
          if Option.is_none x.parse
          then
            if List.for_all ~f:(fun x -> Option.is_some x.parse) xs
            then (
              changed := true;
              let ps =
                List.concat_map ~f:(fun x -> Option.value_exn x.parse) xs
              in
              x.parse <- Some ps);
          List.iter ~f:(update_singlestep_nullables next_tag) xs
        | Char c -> ()
        | Range crl -> ()
        | Reduction (f, x') ->
          (* no reason to be lazy here *)
          update_singlestep_nullables next_tag x';
          x.parse <- Option.map ~f x'.parse)
  in
  let rec update_p_calced (next_tag : tag) (x : t) : unit =
    perform_if_p_tag x next_tag (fun () ->
        match node x with
        | Union ts -> List.iter ~f:(update_p_calced next_tag % Lazy.force) ts
        | Concat ts -> List.iter ~f:(update_p_calced next_tag % Lazy.force) ts
        | Char c -> ()
        | Range crl -> ()
        | Reduction (f, x') -> update_p_calced next_tag x')
  in
  fold_until_completion
    ~f:(fun tag ->
      let new_tag = switch_tag tag in
      if !changed
      then (
        changed := false;
        update_singlestep_nullables new_tag x;
        Left new_tag)
      else Right (update_p_calced new_tag x))
    Red
;;

let nullable (x : t) : bool =
  if x.p_calced
  then Option.is_some x.parse
  else (
    materialize_nullables x;
    Option.is_some x.parse)
;;

let extract_parse (x : t) : NumberedId.t list option =
  if x.p_calced
  then x.parse
  else (
    materialize_nullables x;
    x.parse)
;;

let rec derivative (c : Char.t) (x : t) : t =
  let prior_deriv = CharMap.find x.derivative c in
  match prior_deriv with
  | Some v -> v
  | None ->
    let deriv =
      match node x with
      | Union xs ->
        let next =
          List.map
            ~f:(fun x ->
              Lazy.from_fun (fun () -> derivative c (Lazy.force_val x)))
            xs
        in
        union next
      | Concat xs ->
        let rec concat_disjunction (xs : t Lazy.t list) : t list =
          match xs with
          | [] -> []
          | h :: t ->
            let h = Lazy.force_val h in
            let first_deriv =
              concat (Lazy.from_fun (fun () -> derivative c h) :: t)
            in
            begin
              match extract_parse h with
              | None -> [first_deriv]
              | Some p ->
                let nexts =
                  List.map
                    ~f:(fun d -> reduction (fun bs -> p @ bs) d)
                    (concat_disjunction t)
                in
                first_deriv :: nexts
            end
        in
        union (List.map ~f:Lazy.from_val (concat_disjunction xs))
      | Char c' -> if Char.equal c c' then epsilon else empty
      | Range crl -> if CRList.is_member c crl then epsilon else empty
      | Reduction (f, x) -> reduction f (derivative c x)
    in
    x.derivative <- CharMap.add_exn x.derivative ~key:c ~data:deriv;
    deriv
;;

let compact (x : t) : t =
  let changed = ref true in
  let perform_if_unmaterialized (x : t) (f : unit -> unit) : unit =
    if Option.is_some x.compacted then () else f ()
  in
  let rec single_step_materialize (x : t) : unit =
    perform_if_unmaterialized x (fun () ->
        match node x with
        | Union xs ->
          let xs = List.map ~f:Lazy.force_val xs in
          let len = List.length xs in
          if len = 1
          then (
            changed := true;
            x.compacted <- Some (List.hd_exn xs))
          else (
            let xs = List.filter ~f:(not % is_empty) xs in
            let xs = List.dedup_and_sort ~compare xs in
            let new_len = List.length xs in
            if not (Int.equal len new_len) then changed := true;
            x.compacted
              <- Some
                   (union
                      (List.map
                         ~f:(fun x ->
                           Lazy.from_fun (fun () ->
                               Option.value_exn x.compacted))
                         xs));
            List.iter ~f:single_step_materialize xs)
        | Concat xs ->
          let xs = List.map ~f:Lazy.force xs in
          if List.exists ~f:is_empty xs
          then (
            changed := true;
            x.compacted <- Some empty)
          else (
            let len = List.length xs in
            if len = 1
            then (
              changed := true;
              x.compacted <- Some (List.hd_exn xs))
            else (
              let xs = List.filter ~f:(not % is_epsilon) xs in
              let new_len = List.length xs in
              if not (Int.equal len new_len) then changed := true;
              x.compacted
                <- Some
                     (concat
                        (List.map
                           ~f:(fun x ->
                             Lazy.from_fun (fun () ->
                                 Option.value_exn x.compacted))
                           xs));
              List.iter ~f:single_step_materialize xs))
        | Char _ -> x.compacted <- Some x
        | Range _ -> x.compacted <- Some x
        | Reduction (f, y) ->
          begin
            match node y with
            | Reduction (g, z) ->
              changed := true;
              x.compacted <- Some (reduction (f % g) z);
              single_step_materialize y
            | _ ->
              x.compacted <- Some x;
              single_step_materialize y
          end)
  in
  fold_until_completion
    ~f:(fun x ->
      if !changed
      then (
        changed := false;
        single_step_materialize x;
        Left (Option.value_exn x.compacted))
      else Right x)
    x
;;

let parse (a : t) (s : string) : NumberedId.t list option =
  print_endline "begin parse";
  let rec parse (a : t) (cs : Char.t list) : NumberedId.t list option =
    let a = compact a in
    if is_empty a
    then None
    else begin
      match cs with
      | [] ->
        print_endline "begin extract";
        extract_parse a
      | c :: cs ->
        print_endline "ay";
        let a = derivative c a in
        parse a cs
    end
  in
  let ans = parse a (String.to_list s) in
  print_endline "end parse";
  ans
;;

let accepts (a : t) (s : string) : bool = Option.is_some (parse a s)

let parse_exn (a : t) (s : string) : NumberedId.t list =
  Option.value_exn (parse a s)
;;
