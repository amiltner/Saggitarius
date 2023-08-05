type t = NumberedId.t list

let empty : t = []
let add (a : NumberedId.t) (b : t) : t = a :: b

let remove_duplicates_hashtable (l : t) : t =
  let open List in
  let tbl = Hashtbl.create (length l) in
  iter (fun x -> Hashtbl.replace tbl x ()) l;
  Hashtbl.fold (fun x () xs -> x :: xs) tbl []
;;

let show (x : t) : string =
  let rec helper r =
    match r with
    | [] -> ""
    | [hd] -> NumberedId.show hd ^ "]"
    | hd :: tl -> NumberedId.show hd ^ "," ^ helper tl
  in
  "[" ^ helper x
;;

let combine (a : t) (b : t) : t = remove_duplicates_hashtable (a @ b)
let compare (a : t) (b : t) : int = 0
