(*Implements the delta-debugging algorithm to find the maximal program
 * which fails to derive a given formula.*)
open MyStdLib
open EarleyDefs
open ClunkyGrammar

exception Underivable of string

(*returns an array of n disjoint lists*)
let split_list (l : 'a list) (n : int) : 'a list array =
  let arr = Array.init n ~f:(fun _ -> []) in
  let rec aux (r : 'a list) (i : int) : unit =
    match r with
    | [] -> ()
    | hd :: tl ->
      arr.(i) <- hd :: arr.(i);
      let ni = (i + 1) mod n in
      aux tl ni
  in
  aux l 0;
  arr
;;

let printflag = ref false

(*Returns the smallest sub-provenance that returns true on the 
 * provided test function *)
let deltaDebugger (fullNids : NumberedId.t list) (test : RuleSet.t -> bool)
    : RuleSet.t
  =
  (*checks to see if excluded the following set of theorems 
   * causes the desired result to be underivable*)
  (*n: # of groups; pool: unused theorem still in contention;
   * lastpassed *)
  let rec helper (n : int) (pool : NumberedId.t list) (lastPassed : RuleSet.t)
      : RuleSet.t
    =
    let s = List.length pool in
    let poolRuleSet = RuleSet.from_list pool in
    let arr = split_list pool n in
    let provArr = Array.map ~f:RuleSet.from_list arr in
    (* Direct Loop *)
    let rec first_pass (i : int) : int option =
      if i = n
      then None
      else if test provArr.(i)
      then Some i
      else first_pass (i + 1)
    in
    match first_pass 0 with
    (*Case: An original passes *)
    | Some i ->
      if !printflag then print_endline "Something passed";
      let passed = provArr.(i) in
      let s = RuleSet.size passed in
      if s = 0
      then lastPassed
      else if s = 1
      then passed
      else helper 2 (RuleSet.as_list passed) passed
    (* Complement Loop *)
    | None ->
      let rec first_complement_pass (i : int) : int option =
        if i = n
        then None
        else (
          let comp = RuleSet.diff poolRuleSet provArr.(i) in
          if !printflag
          then print_endline ("Comp size: " ^ string_of_int (RuleSet.size comp));
          if test comp then Some i else first_complement_pass (i + 1))
      in
      (match first_complement_pass 0 with
      (*Case: A complement fails*)
      | Some i ->
        if !printflag then print_endline "Something passed";
        let nfp = RuleSet.diff poolRuleSet provArr.(i) in
        helper (n - 1) (RuleSet.as_list nfp) nfp
      (*Case : Everything Passes*)
      | None ->
        if !printflag then print_endline "Everything failed";
        if n = s
        then lastPassed
        else (
          let m = 2 * n in
          if m > s then helper s pool lastPassed else helper m pool lastPassed))
  in
  helper 2 fullNids (RuleSet.from_list fullNids)
;;
