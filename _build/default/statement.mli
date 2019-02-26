type term = Term of char | Neg of term

type subPred = Plus of term | Minus of term

type t = Statement of { sub : subPred; pred : subPred } | Neg of t

val toString : t -> string

val toStrings : t list -> string
