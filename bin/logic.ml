open Lang

type categoricalTypes = A | E | I | O
(*let string_of_cat = function A -> 'A' | E -> 'E' | I -> 'I' | O -> 'O'*)

(* --------------------- helper functions --------------------- *)
let statementTerms s =
  let rec statementParts = function
    | Statement { sub; pred } -> (sub, pred)
    | Neg s -> statementParts s
  in
  match statementParts s with
  | (Plus a, Plus b) -> (a, b)
  | (Plus a, Minus b) -> (a, b)
  | (Minus a, Plus b) -> (a, b)
  | (Minus a, Minus b) -> (a, b)

let rec statementType = function
  | Neg s -> 
    (match statementType s with 
     | A -> O
     | E -> I
     | I -> E
     | O -> A)
  | Statement { sub; pred } ->
    (match (sub, pred) with
     | (Plus _, Plus _) -> I
     | (Plus _, Minus _) -> O
     | (Minus _, Plus _) -> A
     | (Minus _, Minus _) -> E)

let negateTerm (t:Lang.term) : Lang.term =
  match t with
  | Term _ as t -> Neg t
  | Neg t -> t


(* --------------------- logic functions --------------------- *)
let predicateObverse = function
  | Statement { sub; pred = Plus t } ->
    let term' = negateTerm t in
    Some (Statement { sub = sub; pred = Minus term' })
  | Statement { sub; pred = Minus t } ->
    let term' = negateTerm t in
    Some (Statement { sub = sub; pred = Plus term' })
  | _ -> None

let statementObverse = function
  | Neg ((Statement _) as s) -> 
    let (subTerm, predTerm) = statementTerms s in
    (match statementType s with
    | A -> Some (Statement { sub = Plus subTerm; pred = Minus predTerm })
    | E -> Some (Statement { sub = Plus subTerm; pred = Plus predTerm })
    | I -> Some (Statement { sub = Minus subTerm; pred = Minus predTerm })
    | O -> Some (Statement { sub = Minus subTerm; pred = Plus predTerm }))
  | (Statement _) as s ->
    let (subTerm, predTerm) = statementTerms s in
    (match statementType s with
    | A -> Some (Neg ((Statement { sub = Plus subTerm; pred = Minus predTerm })))
    | E -> Some (Neg (Statement { sub = Plus subTerm; pred = Plus predTerm }))
    | I -> Some (Neg (Statement { sub = Minus subTerm; pred = Minus predTerm }))
    | O -> Some (Neg (Statement { sub = Minus subTerm; pred = Plus predTerm })))
  | _ -> None

let converse = function
  | Statement { sub = Minus predTerm; pred = Plus subTerm } ->
    Some (Statement { sub = Minus subTerm; pred = Plus predTerm })
  | Statement { sub = Minus predTerm; pred = Minus subTerm } ->
    Some (Statement { sub = Minus subTerm; pred = Minus predTerm })
  | Statement { sub = Plus predTerm; pred = Plus subTerm } ->
    Some (Statement { sub = Plus subTerm; pred = Plus predTerm })
  | Statement { sub = Plus predTerm; pred = Minus subTerm } ->
    Some (Statement { sub = Plus subTerm; pred = Minus predTerm })
  | _ -> None

let contrapositive s =
  let appNeg = function
    | Plus t -> Plus (negateTerm t)
    | Minus t -> Minus (negateTerm t)
  in
  match converse s with
  | None -> None
  | Some (Statement { sub; pred }) ->
    Some (Statement { sub = appNeg sub; pred = appNeg pred })
  | Some (Neg _) -> assert false

let addDN = function
  | statement -> Neg (Neg statement)

let removeDN = function
  | Neg (Neg statement) -> Some statement
  | _ -> None

let ddo s1 s2 =
  let ddo matrixSub omniPred =
    Statement { sub = matrixSub; pred = omniPred }
  in
  match (s1, s2) with
  | (Statement { sub = Minus omniSterm; pred = omniP },
     Statement { sub = matrixS; pred = Plus matrixPterm })
    when (Lang.terms_equal omniSterm matrixPterm) -> Some (ddo matrixS omniP)
  | (Statement { sub = matrixS; pred = Plus matrixPterm },
     Statement { sub = Minus omniSterm; pred = omniP })
    when (Lang.terms_equal omniSterm matrixPterm) -> Some (ddo matrixS omniP)
  | _ -> None


let applyIfEquivalent rule operands =
  match (rule, operands) with
  | (Premise, _) -> failwith "cannot apply premise rule"
  | (PO, [rand]) ->
    let app = predicateObverse rand.statement in
    (match app with
    | None -> None
    | Some s -> Some { statement = s
                     ; refs = [rand]
                     ; rule = rule })
  | (SO, [rand]) ->
    let app = statementObverse rand.statement in
    (match app with
    | None -> None
    | Some s -> Some { statement = s
                     ; refs = [rand]
                     ; rule = rule })
  | (Converse, [rand]) ->
    (match statementType rand.statement with
     | I | E ->
       let app = converse rand.statement in
       (match app with
       | None -> None
       | Some s -> Some { statement = s
                        ; refs = [rand]
                        ; rule = rule })
       | _ -> None)
  | (Contrap, [rand]) ->
    (match statementType rand.statement with
     | A | O ->
       let app = contrapositive rand.statement in
       (match app with
        | None -> None
        | Some s -> Some { statement = s
                         ; refs = [rand]
                         ; rule = rule })
     | _ -> None)

  | (ADN, [rand]) -> Some { statement = addDN rand.statement
                          ; refs = [rand]
                          ; rule = rule }

  | (RDN, [rand]) ->
    let app = removeDN rand.statement in
    (match app with
     | None -> None
     | Some s -> Some { statement = s
                      ; refs = [rand]
                      ; rule = rule })

  | (DDO, [rand1; rand2]) ->
    let ddo = ddo rand1.statement rand2.statement in
    (match ddo with
     | None -> None
     | Some der -> Some { statement = der
                        ; refs = operands
                        ; rule = rule })
  | _ ->
    let rands = List.fold_left (fun acc s -> acc ^ (Lang.string_of_statement s.statement)) "" operands in
    let rule = Lang.string_of_rule rule in
    failwith (Printf.sprintf "%s not applicable to arguments %s" rule rands)




