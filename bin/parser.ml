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
    (Neg innerT, tokens)
  | _ -> failwith "invalid term form"

let rec statement tokens =
  match tokens with
  | LPar :: tokens ->
    let (innerS, tokens) = statement tokens in
    (match tokens with
     | RPar :: tokens -> (innerS, tokens)
     | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
  | Minus :: LPar :: tokens ->
    let (innerS, tokens) = statement tokens in
    (match tokens with
     | RPar :: tokens -> (Neg innerS, tokens)
     | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
  | Plus :: tokens ->
    let (sub, tokens) = term tokens in
    (match tokens with
     | Plus :: tokens ->
       let (pred, tokens) = term tokens in
       (Statement { quan = Particular; qual = Affirmative; sub; pred }, tokens)
     | Minus :: tokens ->
       let (pred, tokens) = term tokens in
       (Statement { quan = Particular; qual = Negative; sub; pred }, tokens)
     | _ -> failwith "invalid statement")
   | Minus :: tokens ->
    let (sub, tokens) = term tokens in
    (match tokens with
     | Plus :: tokens ->
       let (pred, tokens) = term tokens in
       (Statement { quan = Universal; qual = Affirmative; sub; pred }, tokens)
     | Minus :: tokens ->
       let (pred, tokens) = term tokens in
       (Statement { quan = Universal; qual = Negative; sub; pred }, tokens)
     | _ -> failwith "invalid statement")
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

