open MyStdLib
open EarleyDefs
open ClunkyGrammar
open NtsToNumber

type gramspec = GrammarSpec.t

let parse (grammar : gramspec) (expression : Expr.t) (startNts : Nts.t)
    : RuleSet.t option
  =
  print_endline "BEGIN PARSE";
  let g = Grammar.from_gramspec grammar startNts in
  let a = Automata.from_grammar g in
  let usedIdsOption = Automata.parse a (String.of_char_list expression) in
  print_endline "END PARSE";
  Option.map ~f:RuleSet.from_list usedIdsOption
;;

let recognize (grammar : gramspec) (expression : Expr.t) (startNts : Nts.t)
    : bool
  =
  let g = Grammar.from_gramspec grammar startNts in
  let a = Automata.from_grammar g in
  let usedIdsOption = Automata.parse a (String.of_char_list expression) in
  Option.is_some usedIdsOption
;;

let print_specs () : unit = ()
let print_specs_experiment () : unit = ()
