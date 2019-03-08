type rule = 
  | Premise
  | PO
  | SO
  | Converse
  | Contrap
  | DDO
let string_of_rule = function
  | Premise -> "premise"
  | PO -> "PO"
  | SO -> "SO"
  | Converse -> "conversion"
  | Contrap -> "contraposition"
  | DDO -> "DDO"

type term = Term of char | Neg of term
let rec string_of_term = function
  | Term c -> Char.escaped c
  | Neg t -> (Token.toString Token.LPar)
             ^ (Token.toString (Token.Minus))
             ^ (string_of_term t)
             ^ (Token.toString Token.RPar)
let rec terms_equal t1 t2 =
  match (t1, t2) with
  | (Term t1, Term t2) -> Char.equal t1 t2
  | (Neg t1, Neg t2) -> terms_equal t1 t2
  | _ -> false

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
let linesIn str =
  let rec linesIn str count start =
    try
      let l = String.index_from str start '\n' in
      linesIn str (count+1) (l+1)
    with Not_found -> count
  in
  linesIn str 0 0
let left (a, _) = a
let right (_, b) = b
let removeEnd ls =
  let ls' = List.mapi (fun i e -> if i = (List.length ls)-1 then None else Some e) ls in
  removeOptions ls'

let string_of_judgement judgement =
  let rec string_of_judgement { statement; refs; rule } startLine =
    let stateStr = string_of_statement statement in
    let ruleStr = string_of_rule rule in
    let lineRefs = List.fold_left (fun lines j -> ((List.hd lines) - linesIn (string_of_judgement j 0)) :: lines) [startLine-1] refs in
    let lineRefs = List.fold_left (fun acc line -> acc ^ (string_of_int line) ^ ", ") "" (List.rev lineRefs) in
    let linesRefs = removeEnd lineRefs in
    let refsStr = List.fold_left (fun acc ref -> 
        let refStr = string_of_judgement ref (right acc) in
        (refStr ^ (left acc), (right acc) - (linesIn refStr))) ("", startLine - 1) refs
    in
    let refsStr = left refsStr in
    Printf.sprintf "%s%d. %s\t(%s%s)\n" refsStr startLine stateStr lineRefs ruleStr
  in
  string_of_judgement judgement 3
