open RuleSet
open MyStdLib
open ProvRecorder
open Constraint.Z

module IntSet = struct
  include SetOf (IntModule)
end

(*These two functions are useful in the SAT-heavy implentation*)
let need_one_SAT (pr : ProvRecorder.t)
    : [> Constraint.Z.zbool ] Constraint.Z.term
  =
  let rec satify (spr : ProvRecorder.t)
      : [> Constraint.Z.zbool ] Constraint.Z.term
    =
    match spr with
    | Or orList ->
      (match orList with
      | [] -> T.false_
      | _ -> T.or_ (List.map ~f:satify orList))
    | And andList ->
      (match andList with
      | [] -> T.true_
      | _ -> T.and_ (List.map ~f:satify andList))
    | Rule i ->
      let numTerm = T.(!(Symbol.declare Bool (string_of_int i))) in
      numTerm
  in
  satify pr
;;

let cant_have_SAT (r : t) : [> Constraint.Z.zbool ] Constraint.Z.term =
  T.not (need_one_SAT r)
;;
