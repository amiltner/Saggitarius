module type Parser = sig
  val parse : DerivParse.gramspec -> Expr.t -> Nts.t -> RuleSet.t option
  val print_specs : unit -> unit
  val print_specs_experiment : unit -> unit
end
