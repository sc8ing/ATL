open Lang
open Token

let rec term (tokens : Token.t list) : Lang.term * Token.t list =
  let _ = Printf.printf "term: %s\n" (Token.toStrings tokens) in
  match tokens with
  | Term e :: STIndicator :: tokens -> (SingleTerm e, tokens)
  | Term e :: tokens -> (Term e, tokens)
  | LPar :: tokens ->
    let (inner, tokens) = innerTerm tokens in
    (match tokens with
     | RPar :: tokens -> (inner, tokens)
     | _ -> failwith "missing rpar")
  | _ -> failwith "invalid term form"

and innerTerm tokens =
  try (* to make combined term *)
    let ctTermPart tokens = 
      match tokens with
      | Plus :: tokens -> (Plus, term tokens)
      | Minus :: tokens -> (Minus, term tokens)
      | _ -> failwith "not cttermpart"
    in
    let (typLeft, (leftT, tokens)) = ctTermPart tokens in
    let (typRight, (rightT, tokens)) = ctTermPart tokens in
    (match (typLeft, typRight) with
     | (Plus, Plus) -> (Intersection (leftT, rightT), tokens)
     | (Plus, Minus) -> (Without (leftT, rightT), tokens)
     | (Minus, Plus) -> (Union (leftT, rightT), tokens)
     | (Minus, Minus) -> (Nor (leftT, rightT), tokens)
     | _ -> failwith "ctTermPart shouldn't return anything other than Plus or Minus")
  with Failure _ -> (* normal term *)
  match tokens with
  | Minus :: Term e :: STIndicator :: tokens -> (Neg (SingleTerm e), tokens)
  | Minus :: Term e :: tokens -> (Neg (Term e), tokens)
  | Minus :: LPar :: tokens ->
    let (innerT, tokens) = innerTerm tokens in
    (match tokens with
     | RPar :: tokens -> (Neg innerT, tokens)
     | _ -> failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
  | LPar :: tokens ->
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
   * maybe recursive descent isn't the best option for this kind of grammar? *)
  | Minus :: LPar :: tokens ->
    (* statement case *)
    (try
       let _ = Printf.printf "ambiguous case, trying to make statement from %s\n" (Token.toStrings tokens) in
       let (innerS, tokens) = statement tokens in
       (match tokens with
        | RPar :: [] ->
          let _ = Printf.printf "successfully made statement from ambiguous case\n" in
          (Neg innerS, tokens) (* no leftover tokens excludes case of combined term *)
        | _ ->
          let _ = Printf.printf "failed to make statement from ambiguous case, leftOvers = %s\n" (Token.toStrings tokens) in
          failwith (Printf.sprintf "missing closing '%s'" (Token.toString Token.RPar)))
     with Failure _ ->
       (* term case *)
       let _ = Printf.printf "failed to make statement, trying to make term from %s\n" (Token.toStrings tokens) in
       let (sub, tokens) = term (LPar :: tokens) in
       let _ = Printf.printf "successfully made term as subject, tokens: %s\n" (Token.toStrings tokens) in
       (match tokens with
        | Plus :: tokens ->
          let (pred, tokens) = term tokens in
          (Statement { quan = Universal; qual = Affirmative; sub; pred }, tokens)
        | Minus :: tokens ->
          let (pred, tokens) = term tokens in
          (Statement { quan = Universal; qual = Negative; sub; pred }, tokens)
        | _ -> failwith "invalid statement")
    )
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

and parse tokens =
  let (result, leftOver) = statement tokens in
  match leftOver with
  | [] -> result
  | _ -> failwith "found parse but extra tokens follow"





