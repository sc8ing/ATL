type rule = 
  | PO
  | SO
  | Converse
  | Contrap

type judgement = Judgement of {
    statement : Statement.t
  ; refs : Statement.t list
  ; rule : rule
  }

type t = judgement list

val toString : t -> string
