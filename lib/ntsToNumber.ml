open MyStdLib

module NtsToNumber = struct
  include DictOf (Nts) (IntList)

  let lookup_helper dict key ~f =
    let res = lookup_default ~default:[] dict key in
    List.map ~f res
  ;;

  let lookup_default_nid dict key =
    lookup_helper dict key (fun i -> NumberedId.create i key)
  ;;
end
