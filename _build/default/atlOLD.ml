type token =
  | LPar
  | RPar
  | Plus
  | Minus
  | Term of char (* elementary term *) (* any way to enfore capitalization in type? *)
  | ConcInd (* conclusion indicator *)

let rec string_of_tokens tokens =
  match tokens with
  | [] -> ""
  | LPar :: tokens-> "(" ^ (string_of_tokens tokens)
  | RPar :: tokens -> ")" ^ (string_of_tokens tokens)
  | Plus :: tokens -> "+" ^ (string_of_tokens tokens)
  | Minus :: tokens -> "-" ^ (string_of_tokens tokens)
  | Term c :: tokens -> (Char.escaped c) ^ (string_of_tokens tokens)
  | ConcInd :: tokens -> "/" ^ (string_of_tokens tokens)

let isUpperCaseChar c =
  let code = Char.code c in
  code >= Char.code 'A' && code <= Char.code 'Z'

let tokenize chars =
  let rec loop tokens chars =
    match chars with
    | [] -> tokens
    | ' ' :: chars | '\n' :: chars -> loop tokens chars
    | '(' :: chars -> loop (LPar :: tokens) chars
    | ')' :: chars -> loop (RPar :: tokens) chars
    | '+' :: chars -> loop (Plus :: tokens) chars
    | '-' :: chars -> loop (Minus :: tokens) chars
    | '/' :: chars -> loop (ConcInd :: tokens) chars
    | c :: chars ->
      if isUpperCaseChar c then loop (Term c :: tokens) chars
      else failwith (Printf.sprintf "invalid character %c" c)
  in
  List.rev (loop [] chars)

(* parser ----------- *)
type term = Term of char | Neg of term
type subPred = Plus of term | Minus of term
type statement = Statement of { sub : subPred; pred : subPred } | Neg of statement

let string_of_statement (Statement { sub = sub; pred = pred }) =
  let rec string_of_term t =
    match t with
    | Term c -> Char.escaped c
    | Neg t -> Printf.sprintf "-(%s)" (string_of_term t)
  in
  let string_of_subPred s =
    match s with
    | Plus e -> Printf.sprintf "+%s" (string_of_term e)
    | Minus e -> Printf.sprintf "-%s" (string_of_term e)
  in
  Printf.sprintf "%s%s" (string_of_subPred sub) (string_of_subPred pred)

let statement tokens = 
  let rec term (tokens : token list) : term * token list =
    match tokens with
    | Term e :: tokens -> (Term e, tokens)
    | LPar :: tokens ->
      let (innerT, tokens) = term tokens in
      (match tokens with
       | RPar :: [] -> (innerT, tokens)
       | _ -> failwith "missing closing ')' or there are extra characters")
    | Minus :: tokens ->
      let (innerT, tokens) = term tokens in
      (match tokens with
       | RPar :: [] -> (Neg innerT, tokens)
       | _ -> failwith "missing closing ')' or there are extra characters")
    | _ -> failwith "invalid term form"
  in
  let rec subPred (tokens : token list) =
    match tokens with
    | Plus :: tokens ->
      let (innerT, tokens) = term tokens in
      (Plus innerT, tokens)
    | Minus :: tokens ->
      let (innerT, tokens) = term tokens in
      (Minus innerT, tokens)
    | _ -> failwith "invalid term prefix"
  in
  let rec statement tokens =
    match tokens with
    | LPar :: tokens ->
      let (innerS, tokens) = statement tokens in
      (match tokens with
       | RPar :: [] -> (innerS, tokens)
       | _ -> failwith "missing closing ')' or there are extra characters")
    | Minus :: LPar :: tokens ->
      let (innerS, tokens) = statement tokens in
      (match tokens with
       | RPar :: [] -> (Neg innerS, tokens)
       | _ -> failwith "missing closing ')' or there are extra characters")
    | Plus :: _ | Minus :: _ ->
      let (sub, tokens) = subPred tokens in
      let (pred, tokens) = subPred tokens in
      (Statement { sub = sub; pred = pred }, tokens)
    | _ -> failwith "invalid statement"
  in
  statement tokens

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l;;

let parse tokens =
  let rec loop premises tokens =
    match tokens with
    | [] -> failwith "missing conclusion"
    | ConcInd :: tokens ->
      let (conclusion, tokens) = statement tokens in
      if List.length tokens != 0 then failwith "extra characters after conclusion"
      else (premises, conclusion)
    | anythingElse ->
      let (premise, tokens) = statement tokens in
      loop (premise :: premises) tokens
  in
  let (premises, conclusion) = loop [] tokens in
  let premises = List.rev premises in
  (premises, conclusion)

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
type derivation = judgement list

let string_of_rule rule =
  match rule with
  | PO -> "PO"
  | SO -> "SO"
  | Converse -> "conversion"
  | Contrap -> "contraposition"

let string_of_judgement (Judgement {
      statement = statement
    ; refs = refs
    ; rule = rule }) =
  let statement = string_of_statement statement in
  let refs = List.map string_of_statement refs in
  let refs = List.fold_left (fun acc s -> acc ^ s ^ ",") "" refs in
  let rule = string_of_rule rule in
  Printf.sprintf "%s\t(%s %s)" statement refs rule

let string_of_derivation derivation =
  let judgements = List.map string_of_judgement derivation in
  List.fold_left (fun acc s -> acc ^ s ^ "\n") "" judgements


let eval premises conclusion =
  Some [ Judgement { statement = List.nth premises 0
                   ; refs = premises
                   ; rule = PO }
       ]

let rec go () =
  let rec loop s =
    try
      let line = read_line () in
      loop (s ^ line)
    with End_of_file -> s
  in
  let input = loop "" in
  let input = tokenize (explode input) in
  let (premises, conclusion) = parse input in
  let valid = eval premises conclusion in
  let valid =
    match valid with
    | None -> "not valid"
    | Some derivation -> string_of_derivation derivation
  in
  let premises = List.map (fun x -> (string_of_statement x) ^ "\n") premises in
  let premises = List.fold_left (fun acc s -> acc ^ s) "" premises in
  let conclusion = "/ " ^ string_of_statement conclusion in
  let _ = Printf.printf "%s%s\n%s" premises conclusion valid in
  go ()









