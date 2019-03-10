open Lang
open Token

let rec term (tokens : Token.t list) : Lang.term * Token.t list =
  match tokens with
  | Term e :: STIndicator :: tokens -> (SingleTerm e, tokens)
  | Term e :: tokens -> (Term e, tokens)
  | LPar :: tokens ->
    let (innerT, tokens) = innerTerm tokens in
    (match tokens with
     | RPar :: tokens -> (innerT, tokens)
     | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
  | _ -> failwith "invalid term form"

and innerTerm = function
  | Minus :: Term e :: STIndicator :: tokens -> (Neg (SingleTerm e), tokens)
  | Minus :: Term e :: tokens -> (Neg (Term e), tokens)
  | Minus :: LPar :: tokens ->
    let (innerT, tokens) = innerTerm tokens in
    (match tokens with
     | RPar :: tokens -> (innerT, tokens)
     | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
  | tokens -> term tokens

let rec statement tokens =
  match tokens with
  | LPar :: tokens ->
    let (innerS, tokens) = statement tokens in
    (match tokens with
     | RPar :: tokens -> (innerS, tokens)
     | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
  (* this case is ambiguous to the parser because both of the following can be statements:
   * -(+A+B) and -(-A)+B
   * ~ -(statement) and -term+term
   * maybe recursive descent isn't the best option for this kind of grammar *)
  | Minus :: LPar :: tokens ->
    (* statement case *)
    (try
       let (innerS, tokens) = statement tokens in
       (match tokens with
        | RPar :: tokens -> (Neg innerS, tokens)
        | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
     with Failure _ ->
     (* term case *)
     try
       let (sub, tokens) = term (LPar :: tokens) in
       (match tokens with
        | Plus :: tokens ->
          let (pred, tokens) = term tokens in
          (Statement { quan = Universal; qual = Affirmative; sub; pred }, tokens)
        | Minus :: tokens ->
          let (pred, tokens) = term tokens in
          (Statement { quan = Universal; qual = Negative; sub; pred }, tokens)
        | _ -> failwith "invalid statement")
     with Failure _ -> failwith "bad statement")
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

