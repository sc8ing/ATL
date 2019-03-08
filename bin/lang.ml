type rule = 
  | Premise
  | PO
  | SO
  | Converse
  | Contrap
let string_of_rule = function
  | Premise -> "premise"
  | PO -> "PO"
  | SO -> "SO"
  | Converse -> "conversion"
  | Contrap -> "contraposition"

type term = Term of char | Neg of term
let rec string_of_term = function
  | Term c -> Char.escaped c
  | Neg t -> (Token.toString Token.LPar)
             ^ (Token.toString (Token.Minus))
             ^ (string_of_term t)
             ^ (Token.toString Token.RPar)

type subPred = Plus of term | Minus of term
let string_of_subPred = function
  | Plus t -> (Token.toString Token.Plus) ^ (string_of_term t)
  | Minus t -> (Token.toString Token.Minus) ^ (string_of_term t)

type statement =
  | Statement of { sub : subPred; pred : subPred }
  | Neg of statement
let rec string_of_statement = function
  | Statement { sub; pred } -> (string_of_subPred sub) ^ (string_of_subPred pred)
  | Neg statement -> (Token.toString Token.Minus)
                     ^ (Token.toString Token.LPar)
                     ^ string_of_statement statement
                     ^ (Token.toString Token.RPar)

type judgement = { statement : statement
                 ; refs : judgement list
                 ; rule : rule }
let rec string_of_judgement { statement; refs; rule } =
  let refs =
    if rule = Premise then ""
    else List.fold_left (fun acc s -> acc ^ (string_of_judgement s) ^ ",") "" refs
  in
  let statement = string_of_statement statement in
  let rule = string_of_rule rule in
  Printf.sprintf "%s\t(%s %s)" statement refs rule
let string_of_judgements judgements =
  List.fold_left (fun acc s -> acc ^ (string_of_judgement s) ^ "\n") "" judgements
