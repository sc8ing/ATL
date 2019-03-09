type rule = 
  | Premise
  | Supposition
  | PO
  | SO
  | Converse
  | Contrap
  | ADN
  | RDN
  | ST
  | DDO
val string_of_rule : rule -> string

type term = SingleTerm of char | Term of char | Neg of term
val string_of_term : term -> string

type quantity = Universal | Particular
type quality = Affirmative | Negative

type statement = Statement of { quan : quantity; qual : quality; sub : term; pred : term }
               | Neg of statement
val string_of_statement : statement -> string

type judgement = { statement : statement
                 ; refs : judgement list
                 ; rule : rule }
val string_of_judgement : judgement -> string
