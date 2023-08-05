module type Recognizer = sig
  val recognize : DerivParse.gramspec -> Expr.t -> Nts.t -> bool
  val print_specs : unit -> unit
  val print_specs_experiment : unit -> unit
end
