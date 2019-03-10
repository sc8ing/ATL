open Lang

type categoricalTypes = A | E | I | O
(*let string_of_cat = function A -> 'A' | E -> 'E' | I -> 'I' | O -> 'O'*)

(* --------------------- helper functions --------------------- *)

let rec statementType = function
  | Neg s -> 
    (match statementType s with 
     | A -> O
     | E -> I
     | I -> E
     | O -> A)
  | Statement { quan; qual; _ } ->
    (match (quan, qual) with
     | (Universal, Affirmative) -> A
     | (Universal, Negative) -> E
     | (Particular, Affirmative) -> I
     | (Particular, Negative) -> O)

let negateQuality = function Affirmative -> Negative | Negative -> Affirmative
let negateQuantity = function Universal -> Particular | Particular -> Universal

let negateTerm (t:Lang.term) : Lang.term =
  match t with
  | Neg t -> t
  | nonNeg -> Neg nonNeg

let negateStatement = function
  | Neg s -> s
  | Statement _ as s -> Neg s

let categoricallyContradicts s1 s2 =
  match (s1, s2) with
  | (Statement s1i, Statement s2i) ->
    if s1i.sub <> s2i.sub || s1i.pred <> s2i.pred then false
    else
      (match (statementType s1, statementType s2) with
       | (A, O) | (E, I) | (I, E) | (O, A) -> true
       | _ -> false)
  | _ -> false

let directlyContradicts s1 s2 =
  match (s1, s2) with
  | (Neg s1, (Statement _ as s2)) -> s1 = s2
  | (Statement _ as s1, Neg s2) -> s1 = s2
  | _ -> false

let contradicts s1 s2 =
  categoricallyContradicts s1 s2 || directlyContradicts s1 s2

let categoricalContradictory = function
  | Statement { quan; qual; sub; pred } ->
    Some (Statement { quan = negateQuantity quan
                    ; qual = negateQuality qual
                    ; sub
                    ; pred })
  | _ -> None

(* ---------------- logic functions (corresponding to a rule) ----------------- *)
let predicateObverse = function
  | Statement { quan; qual; sub; pred } ->
    Some (Statement { quan
                    ; qual = negateQuality qual
                    ; sub
                    ; pred = negateTerm pred })
  | _ -> None

let statementObverse = function
  | Statement _ as s ->
    let catCon = categoricalContradictory s in
    (match catCon with
     | None -> failwith "shouldn't happen"
     | Some c -> Some (Neg c))
  | Neg (Statement _ as s) ->
    let catCon = categoricalContradictory s in
    (match catCon with
     | None -> failwith "shouldn't happen"
     | Some c -> Some c)
  | _ -> None

let converse = function
  | Statement { quan; qual; sub; pred } ->
    Some (Statement { quan
                    ; qual
                    ; sub = pred
                    ; pred = sub })
  | _ -> None

let contrapositive = function
  | Statement { quan; qual; sub; pred } ->
    Some (Statement { quan
                    ; qual
                    ; sub = negateTerm pred
                    ; pred = negateTerm sub })
  | _ -> None

let addDN = function
  | statement -> Neg (Neg statement)

let removeDN = function
  | Neg (Neg statement) -> Some statement
  | _ -> None

let universalizeST = function
  | Statement { quan = Particular
              ; qual
              ; sub = SingleTerm _ as sub
              ; pred } ->
    Some (Statement { quan = Universal
                    ; qual
                    ; sub
                    ; pred })
  | _ -> None

let ddo s1 s2 =
  match (s1, s2) with
  | (Statement { quan; qual = Affirmative; sub; pred = mPred },
     Statement { quan = Universal; qual; sub = oSub; pred }) when oSub = mPred ->
    Some (Statement { quan; qual; sub; pred })
  | (Statement { quan = Universal; qual; sub = oSub; pred },
     Statement { quan; qual = Affirmative; sub; pred = mPred }) when oSub = mPred ->
    Some (Statement { quan; qual; sub; pred })
  | _ -> None


let applyIfEquivalent rule operands =
  match (rule, operands) with
  | (Premise, _) -> failwith "cannot apply premise rule"
  | (Supposition, _) -> failwith "cannot apply supposition rule"
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

  | (ST, [rand]) ->
    let app = universalizeST rand.statement in
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




