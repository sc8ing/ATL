open Lang
open Token

let rec term (tokens : Token.t list) : Lang.term * Token.t list =
  match tokens with
  | Term e :: STIndicator :: tokens -> (SingleTerm e, tokens)
  | Term e :: tokens -> (Term e, tokens)
  | LPar :: tokens ->
    (* combined terms first *)
    (try
      let ctTermPart tokens =
        match tokens with
        | Plus :: tokens ->
          let (t, tokens) = term tokens in
          (Plus, t, tokens)
        | Minus :: tokens ->
          let (t, tokens) = term tokens in
          (Minus, t, tokens)
        | _ -> failwith "not a combined term part"
      in
      let (typLeft, leftTerm, tokens) = ctTermPart tokens in
      let (typRight, rightTerm, tokens) = ctTermPart tokens in
      match tokens with
      | RPar :: tokens ->
        (match (typLeft, typRight) with
         | (Plus, Plus) -> (Intersection (leftTerm, rightTerm), tokens)
         | (Plus, Minus) -> (Without (leftTerm, rightTerm), tokens)
         | (Minus, Plus) -> (Union (leftTerm, rightTerm), tokens)
         | (Minus, Minus) -> (Nor (leftTerm, rightTerm), tokens)
         | _ -> failwith "should not be possible")
      | _ -> failwith ("combined term missing closing " ^ (Token.toString Token.RPar))
    (* then try normal terms *)
    with Failure _ ->
      let (innerT, tokens) = innerTerm tokens in
      (match tokens with
       | RPar :: tokens -> (innerT, tokens)
       | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar))))
  | _ -> failwith "invalid term form"

and innerTerm = function
  | Minus :: Term e :: STIndicator :: tokens -> (Neg (SingleTerm e), tokens)
  | Minus :: Term e :: tokens -> (Neg (Term e), tokens)
  | Minus :: LPar :: tokens ->
    let (innerT, tokens) = innerTerm tokens in
    (match tokens with
     | RPar :: tokens -> (Neg innerT, tokens)
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
   * maybe recursive descent isn't the best option for this kind of grammar? *)
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
  let (result, leftOver) = statement tokens in
  match leftOver with
  | [] -> result
  | _ -> failwith "found parse but extra tokens follow"





