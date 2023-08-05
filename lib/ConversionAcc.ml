open MyStdLib

type t =
  { ips : IndexedProduction.t list
  ; cs : Constraint.t list
  ; index : int
  ; index_map : (Nts.t * int) list
  ; concretized_map : (int * (Nts.t * ConcretizedRegex.t)) list
  }

let add_ip (old : t) (ip : IndexedProduction.t) : t =
  { old with ips = ip :: old.ips }
;;

let update
    (old : t)
    (new_ips : IndexedProduction.t list)
    (new_constraints : Constraint.t list)
    (new_index : int)
    (new_maps : (Nts.t * int) list)
    (new_concretized : (int * (Nts.t * ConcretizedRegex.t)) list)
    : t
  =
  { ips = new_ips @ old.ips
  ; cs = new_constraints @ old.cs
  ; index = new_index
  ; index_map = new_maps @ old.index_map
  ; concretized_map = new_concretized @ old.concretized_map
  }
;;

let empty =
  { ips = []; cs = []; index = 0; index_map = []; concretized_map = [] }
;;

let add_name_at_index
    (env : (Id.t * int) list)
    (acc : t)
    (ido : IndexedId.t option)
    : t
  =
  match ido with
  | None -> acc
  | Some (name, indices) ->
    let index_vals = List.map ~f:(IntExp.eval env) indices in
    { acc with index_map = ((name, index_vals), acc.index) :: acc.index_map }
;;

let increment_index (acc : t) : t = { acc with index = acc.index + 1 }

let add_concretized
    (acc : t)
    (num : int)
    (nts : Nts.t)
    (cr : ConcretizedRegex.t)
    : t
  =
  { acc with concretized_map = (num, (nts, cr)) :: acc.concretized_map }
;;
