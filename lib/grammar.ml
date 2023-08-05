open MyStdLib
open ClunkyGrammar
open CharRange
module OtherProduction = Production

module Production = struct
  type t =
    | Nonterminal of Nts.t
    | Terminal of string
    | Concat of t * t
    | Or of t * t
    | Empty
    | Labelled of NumberedId.t * t
    | Range of CRList.t
  [@@deriving bin_io, eq, hash, ord, sexp, show]

  let from_other_production (o : OtherProduction.t) : t =
    let psymbs =
      List.map
        ~f:(fun s ->
          match s with
          | Terminal crl -> Range crl
          | Nonterminal nts -> Nonterminal nts)
        o
    in
    fold_on_head_with_default
      ~f:(fun p1 p2 -> Concat (p1, p2))
      ~default:(Terminal "")
      psymbs
  ;;

  let mk_nonterminal (nts : Nts.t) : t = Nonterminal nts
  let mk_terminal (s : string) : t = Terminal s
  let mk_or (p1 : t) (p2 : t) : t = Or (p1, p2)
  let mk_concat (p1 : t) (p2 : t) : t = Concat (p1, p2)
  let empty : t = Empty

  let apply_nonterminal (type a) ~(f : Nts.t -> a) (s : t) : a option =
    match s with
    | Nonterminal i -> Some (f i)
    | _ -> None
  ;;

  let extract_nonterminal : t -> Nts.t option = apply_nonterminal ~f:ident

  let extract_nonterminal_exn (s : t) : Nts.t =
    Option.value_exn (extract_nonterminal s)
  ;;

  let apply_terminal (type a) ~(f : string -> a) (s : t) : a option =
    match s with
    | Terminal s -> Some (f s)
    | _ -> None
  ;;

  let extract_terminal : t -> string option = apply_terminal ~f:ident

  let extract_terminal_exn (s : t) : string =
    Option.value_exn (extract_terminal s)
  ;;

  let apply_concat (type a) ~(f : t -> t -> a) (s : t) : a option =
    match s with
    | Concat (c1, c2) -> Some (f c1 c2)
    | _ -> None
  ;;

  let extract_concat : t -> (t * t) option = apply_concat ~f:Tuple.T2.create
  let extract_concat_exn (s : t) : t * t = Option.value_exn (extract_concat s)

  let apply_or (type a) ~(f : t -> t -> a) (s : t) : a option =
    match s with
    | Or (c1, c2) -> Some (f c1 c2)
    | _ -> None
  ;;

  let extract_or : t -> (t * t) option = apply_or ~f:Tuple.T2.create
  let extract_or_exn (s : t) : t * t = Option.value_exn (extract_or s)

  let fold
      (type a)
      ~(nonterminal_f : Nts.t -> a)
      ~(terminal_f : string -> a)
      ~(concat_f : a -> a -> a)
      ~(or_f : a -> a -> a)
      ~(empty_f : a)
      ~(labelled_f : NumberedId.t -> a -> a)
      ~(crlist : CRList.t -> a)
      (s : t)
      : a
    =
    let rec fold_internal (s : t) : a =
      match s with
      | Nonterminal i -> nonterminal_f i
      | Terminal s -> terminal_f s
      | Concat (s1, s2) -> concat_f (fold_internal s1) (fold_internal s2)
      | Or (s1, s2) -> or_f (fold_internal s1) (fold_internal s2)
      | Labelled (i, l) -> labelled_f i (fold_internal l)
      | Empty -> empty_f
      | Range cl -> crlist cl
    in
    fold_internal s
  ;;
end

module ProdDict = DictOf (Nts) (Production)

type t =
  { dict : ProdDict.t
  ; start : Production.t
  }
[@@deriving eq, hash, ord, show]

let get_productions (d : t) (s : Nts.t) : Production.t =
  match ProdDict.lookup d.dict s with
  | None -> failwith "bad production"
  | Some ps -> ps
;;

let get_initial_production (d : t) : Production.t = d.start

let add_production (d : t) (s : Nts.t) (p : Production.t) =
  let dict =
    ProdDict.insert_or_combine ~combiner:(fun x y -> failwith "ahh") d.dict s p
  in
  { d with dict }
;;

let update_start (g : t) (p : Production.t) : t = { g with start = p }

let symbol_production_list (g : t) : Production.t * (Nts.t * Production.t) list =
  let start = g.start in
  let list = ProdDict.as_kvp_list g.dict in
  start, list
;;

let initialize (start : Production.t) : t = { dict = ProdDict.empty; start }

let from_kvp_list_exn (start : Production.t) (rs : (Nts.t * Production.t) list)
    : t
  =
  List.fold
    ~f:(fun g (i, ps) -> add_production g i ps)
    ~init:(initialize start)
    rs
;;

let from_gramspec (gramspec : GrammarSpec.t) (startNts : Nts.t) : t =
  let add_production
      (d : ProdDict.t)
      (nid : NumberedId.t)
      (p : OtherProduction.t)
    =
    ProdDict.insert_or_combine
      ~combiner:(fun x y -> Production.Or (x, y))
      d
      nid.id
      (Production.Labelled (nid, Production.from_other_production p))
  in
  let d =
    List.fold
      ~f:(fun g (i, ps) -> add_production g i ps)
      ~init:ProdDict.empty
      (GrammarSpec.as_kvp_list gramspec)
  in
  { dict = d; start = Production.Nonterminal startNts }
;;

let rec materialize (g : t) (p : Production.t) : Production.t =
  match p with
  | Nonterminal v -> materialize g (get_productions g v)
  | _ -> p
;;

let is_recursive_production_modulo
    (g : t)
    (modulo : Production.t list)
    (search : Production.t)
    : bool
  =
  let rec is_recursive_production_modulo
      (p : Production.t)
      (processed : Production.t list)
      : bool
    =
    if Production.equal p search && not (List.is_empty processed)
    then true
    else if List.mem ~equal:Production.equal processed p
    then false
    else if List.mem ~equal:Production.equal modulo p
    then false
    else (
      let processed = p :: processed in
      match p with
      | Nonterminal i ->
        is_recursive_production_modulo (get_productions g i) processed
      | Terminal s -> false
      | Range _ -> false
      | Concat (p1, p2) ->
        is_recursive_production_modulo p1 processed
        || is_recursive_production_modulo p2 processed
      | Or (p1, p2) ->
        is_recursive_production_modulo p1 processed
        || is_recursive_production_modulo p2 processed
      | Empty -> false
      | Labelled (_, p) -> is_recursive_production_modulo p processed)
  in
  is_recursive_production_modulo search []
;;

let get_all_subproductions (g : t) : Production.t -> Production.t list =
  let rec get_all_productions_internal
      (acc : Production.t list)
      (p : Production.t)
      : Production.t list
    =
    if List.mem ~equal:Production.equal acc p
    then acc
    else (
      let acc = p :: acc in
      match p with
      | Nonterminal i -> get_all_productions_internal acc (get_productions g i)
      | Terminal s -> acc
      | Concat (p1, p2) ->
        let acc = get_all_productions_internal acc p1 in
        get_all_productions_internal acc p2
      | Or (p1, p2) ->
        let acc = get_all_productions_internal acc p1 in
        get_all_productions_internal acc p2
      | Empty -> acc
      | Range _ -> acc
      | Labelled (_, p) -> get_all_productions_internal acc p)
  in
  get_all_productions_internal []
;;

let retrieve_necessary_productions (g : t) : Nts.t list =
  let rec retrieve_necessary_productions
      (p : Production.t)
      (processed : Nts.t list)
      : Nts.t list
    =
    match p with
    | Nonterminal i ->
      if List.mem ~equal:Nts.equal processed i
      then processed
      else retrieve_necessary_productions (get_productions g i) (i :: processed)
    | Terminal s -> processed
    | Range _ -> processed
    | Concat (p1, p2) ->
      let processed = retrieve_necessary_productions p1 processed in
      retrieve_necessary_productions p2 processed
    | Or (p1, p2) ->
      let processed = retrieve_necessary_productions p1 processed in
      retrieve_necessary_productions p2 processed
    | Empty -> processed
    | Labelled (_, p) -> retrieve_necessary_productions p processed
  in
  retrieve_necessary_productions g.start []
;;

let remove_unnecessary_productions (g : t) : t =
  let necessary_ids = retrieve_necessary_productions g in
  let kvps = ProdDict.as_kvp_list g.dict in
  let kvps =
    List.filter ~f:(List.mem ~equal:Nts.equal necessary_ids % fst) kvps
  in
  let dict = ProdDict.from_kvp_list kvps in
  { g with dict }
;;

let minify (g : t) : t =
  let rec minify (g : t) (processed : Nts.t list) : t =
    let p = g.start in
    match p with
    | Nonterminal i ->
      if List.mem ~equal:Nts.equal processed i
      then g
      else if is_recursive_production_modulo
                g
                (List.map ~f:Production.mk_nonterminal processed)
                p
      then (
        let prod = get_productions g i in
        let g = update_start g prod in
        let g = minify g (i :: processed) in
        let dict = g.dict in
        let dict = ProdDict.insert dict i g.start in
        { dict; start = p })
      else (
        let prod = get_productions g i in
        let g = update_start g prod in
        minify g processed)
    | Terminal s -> g
    | Range _ -> g
    | Concat (p1, p2) ->
      let g1 = minify (update_start g p1) processed in
      let g2 = minify (update_start g1 p2) processed in
      let p1 = g1.start in
      let p2 = g2.start in
      { start = Production.mk_concat p1 p2; dict = g2.dict }
    | Or (p1, p2) ->
      let g1 = minify (update_start g p1) processed in
      let g2 = minify (update_start g1 p2) processed in
      let p1 = g1.start in
      let p2 = g2.start in
      { start = Production.mk_or p1 p2; dict = g2.dict }
    | Empty -> g
    | Labelled (i, p) ->
      let g = minify (update_start g p) processed in
      let p = g.start in
      { start = Labelled (i, p); dict = g.dict }
  in
  remove_unnecessary_productions (minify g [])
;;

(*let rewrites
    (g:t)
  : t list =
  let rec rewrites
      (g:t)
      (processed:Nts.t list)
    : t list * Nts.t list =
    let p = g.start in
    begin match p with
      | Nonterminal i ->
        let prod = get_productions g i in
        let current_rewrite = update_start g prod in
        let prod = get_productions g i in
        let g = update_start g prod in
        let sub_rewrites =
          if List.mem ~equal:Id.equal processed i then
            []
          else
            let (gs,processed) =
              rewrites
                g
                (i::processed)
            in
            List.map
              ~f:(fun g ->
                  let dict = g.dict in
                  let dict =
                    ProdDict.insert
                      dict
                      i
                      g.start
                  in
                  {
                    dict = dict;
                    start = p;
                  })
              gs
        in
        (current_rewrite::sub_rewrites,i::processed)
      | Terminal s ->
        ([],processed)
      | Concat (p1,p2) ->
        let (g1s,processed) = rewrites (update_start g p1) processed in
        let (g2s,processed) = rewrites (update_start g p2) processed in
        let left_rewrites =
          List.map
            ~f:(fun g1 ->
                let p1 = g1.start in
                {
                  start = Production.mk_concat p1 p2 ;
                  dict = g1.dict ;
                })
            g1s
        in
        let right_rewrites =
          List.map
            ~f:(fun g2 ->
                let p2 = g2.start in
                {
                  start = Production.mk_concat p1 p2 ;
                  dict = g2.dict ;
                })
            g2s
        in
        (left_rewrites@right_rewrites,processed)
      | Or (p1,p2) ->
        let (g1s,processed) = rewrites (update_start g p1) processed in
        let (g2s,processed) = rewrites (update_start g p2) processed in
        let left_rewrites =
          List.map
            ~f:(fun g1 ->
                let p1 = g1.start in
                {
                  start = Production.mk_or p1 p2 ;
                  dict = g1.dict ;
                })
            g1s
        in
        let right_rewrites =
          List.map
            ~f:(fun g2 ->
                let p2 = g2.start in
                {
                  start = Production.mk_or p1 p2 ;
                  dict = g2.dict ;
                })
            g2s
        in
        (left_rewrites@right_rewrites,processed)
      | Empty -> ([],processed)
    end
  in
  fst (rewrites g [])*)

let rec concretify (g : t) (p : Production.t) : Production.t =
  match Production.extract_nonterminal p with
  | None -> p
  | Some i -> concretify g (get_productions g i)
;;
