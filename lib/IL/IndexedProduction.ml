open MyStdLib

type t =
  { pid : int
  ; required : bool
  ; head : Nts.t
  ; symbols : Production.t
  }
[@@deriving eq, hash, ord, show]

let to_string (ip : t) : string = string_of_int ip.pid

let list_to_pm (l : t list) : (Nts.t * int list) list =
  let ntspids = List.map ~f:(fun x -> x.head, x.pid) l in
  group_by_keys ~is_eq:Nts.equal ntspids
;;

let make
    ~(pid : int)
    ~(required : bool)
    ~(head : Nts.t)
    ~(symbols : Symbol.t list)
    : t
  =
  { pid; required; head; symbols }
;;
