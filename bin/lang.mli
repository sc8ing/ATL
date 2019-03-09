type rule = 
  | Premise
  | PO
  | SO
  | Converse
  | Contrap
  | ADN
  | RDN
  | DDO
val string_of_rule : rule -> string

type term = Term of char | Neg of term
val string_of_term : term -> string
val terms_equal : term -> term -> bool

type subPred = Plus of term | Minus of term
val string_of_subPred : subPred -> string

type statement = Statement of { sub : subPred; pred : subPred } | Neg of statement
val string_of_statement : statement -> string

type judgement = { statement : statement
                 ; refs : judgement list
                 ; rule : rule }
val string_of_judgement : judgement -> string
