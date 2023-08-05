open MyStdLib

type t =
  | CharSet of CharRange.CRList.t
  | Disjunct of t list
  | Conjunct of t list
  | Star of t
  | Nonterminal of Nts.t
[@@deriving eq, hash, ord, show, variants]
