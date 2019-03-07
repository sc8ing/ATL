open Lang

let statementType = function
  | Neg s -> 
  | Statement { sub; pred } ->

let predicateObverse {

let apply rule operands =
  match (rule, operands) with
  | (Premise, _) -> failwith "cannot apply premise rule"
  | (PO, [rand]) -> Some { statement = predicateObverse rand.statement
                         ; refs = [rand]
                         ; rule = rule }
  | (SO, [rand]) -> Some { statement = statementObverse rand.statement
                         ; refs = [rand]
                         ; rule = rule }
  | (Converse, [rand]) ->
    (match statementType rand with
     | I | E ->  Some { statement = converse rand.statement
                      ; refs = [rand]
                      ; rule = rule }
     | _ -> None)
  | (Contrap, [rand]) ->
    (match statementType rand with
     | A | O ->  Some { statement = contrapositive rand.statement
                      ; refs = [rand]
                      ; rule = rule }
     | _ -> None)
  | _ ->
    let rands = Lang.string_of_judgements rands in
    let rule = Lang.string_of_rule rule in
    failwith (Printf.sprintf "%s not applicable to arguments %s" rule rands




