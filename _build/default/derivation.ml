type rule = 
  | PO
  | SO
  | Converse
  | Contrap

type judgement = Judgement of {
    statement : statement
  ; refs : statement list
  ; rule : rule
  }

type t = judgement list

let toString derivation =
  let string_of_rule rule =
    match rule with
    | PO -> "PO"
    | SO -> "SO"
    | Converse -> "conversion"
    | Contrap -> "contraposition"
  in
  let string_of_judgement (Judgement {
      statement = statement
    ; refs = refs
    ; rule = rule }) =
    let statement = Statement.toString statement in
    let refs = List.map string_of_statement refs in
    let refs = List.fold_left (fun acc s -> acc ^ s ^ ",") "" refs in
    let rule = string_of_rule rule in
    Printf.sprintf "%s\t(%s %s)" statement refs rule
  in
  let judgements = List.map string_of_judgement derivation in
  List.fold_left (fun acc s -> acc ^ s ^ "\n") "" judgements



