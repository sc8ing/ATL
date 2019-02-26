type rule = 
  | PO
  | SO
  | Converse
  | Contrap
let string_of_rule = function
  | PO -> "PO"
  | SO -> "SO"
  | Converse -> "conversion"
  | Contrap -> "contraposition"

type term = Term of char | Neg of term
let rec string_of_term = function
  | Term c -> Char.escaped c
  | Neg t -> (Token.toString (Token.Minus)) ^ (string_of_term t)

type subPred = Plus of term | Minus of term
let string_of_subPred = function
  | Plus t -> (Token.toString Token.Plus) ^ (string_of_term t)
  | Minus t -> (Token.toString Token.Minus) ^ (string_of_term t)

type statement =
  | Statement of { sub : subPred; pred : subPred }
  | Neg of statement
let rec string_of_statement = function
  | Statement { sub; pred } -> (string_of_subPred sub) ^ (string_of_subPred pred)
  | Neg statement -> (Token.toString Token.Minus) ^ string_of_statement statement

type judgement = Judgement of { statement : statement
                              ; refs : statement list
                              ; rule : rule }
let string_of_judgement (Judgement { statement
                        ; refs
                        ; rule }) =
  let statement = string_of_statement statement in
  let refs = List.fold_left (fun acc s -> acc ^ (string_of_statement s) ^ ",") "" refs in
  let rule = string_of_rule rule in
  Printf.sprintf "%s\t(%s %s)" statement refs rule

type derivation = judgement list
let string_of_derivation derivation =
  List.fold_left (fun acc s -> acc ^ (string_of_judgement s) ^ "\n") "" derivation
