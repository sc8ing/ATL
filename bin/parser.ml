open Lang
open Token

let rec term (tokens : Token.t list) : Lang.term * Token.t list =
  match tokens with
  | Term e :: tokens -> (Term e, tokens)
  | LPar :: tokens ->
    let (innerT, tokens) = term tokens in
    (match tokens with
     | RPar :: tokens -> (innerT, tokens)
     | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
  | Minus :: tokens ->
    let (innerT, tokens) = term tokens in
    (match tokens with
     | RPar :: tokens -> (Neg innerT, tokens)
     | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
  | _ -> failwith "invalid term form"

let subPred (tokens : Token.t list) : Lang.subPred * Token.t list =
  match tokens with
  | Plus :: tokens ->
    let (innerT, tokens) = term tokens in
    (Plus innerT, tokens)
  | Minus :: tokens ->
    let (innerT, tokens) = term tokens in
    (Minus innerT, tokens)
  | _ -> failwith "invalid term prefix"

let rec statement tokens =
  match tokens with
  | LPar :: tokens ->
    let (innerS, tokens) = statement tokens in
    (match tokens with
     | RPar :: tokens -> (Printf.printf "Parser: statement: %s" (Lang.string_of_statement innerS)) ; (innerS, tokens)
     | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
  | Minus :: LPar :: tokens ->
    let (innerS, tokens) = statement tokens in
    (match tokens with
     | RPar :: tokens -> (Neg innerS, tokens)
     | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
  | Plus :: _ | Minus :: _ ->
    let (sub, tokens) = subPred tokens in
    let (pred, tokens) = subPred tokens in
    (Statement { sub = sub; pred = pred }, tokens)
  | _ -> failwith "invalid statement"

let parse tokens =
  let rec loop premises tokens =
    match tokens with
    | [] -> failwith "missing conclusion"
    | ConcInd :: tokens ->
      let (conclusion, tokens) = statement tokens in
      if List.length tokens != 0 then failwith "extra characters after conclusion"
      else (premises, conclusion)
    | anythingElse ->
      let (premise, tokens) = statement anythingElse in
      loop (premise :: premises) tokens
  in
  let (premises, conclusion) = loop [] tokens in
  let premises = List.rev premises in
  (premises, conclusion)

