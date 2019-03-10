type rule = 
  | Premise
  | Supposition
  | PO
  | SO
  | Converse
  | Contrap
  | ADN
  | RDN
  | ST
  | DDO
let string_of_rule = function
  | Premise -> "premise"
  | Supposition -> "supposition"
  | PO -> "PO" (* predicate obversion *)
  | SO -> "SO" (* statement obversion *)
  | Converse -> "conversion"
  | Contrap -> "contraposition"
  | ADN -> "add DN" (* double negation *)
  | RDN -> "remove DN"
  | ST -> "single term"
  | DDO -> "DDO" (* dictum de omni *)

type term =
  | Term of char
  | SingleTerm of char
  | Neg of term
  | Intersection of term * term
  | Without of term * term
  | Union of term * term
  | Nor of term * term
let rec string_of_term = function
  | Term c -> Char.escaped c
  | SingleTerm c -> Char.escaped c ^ (Token.toString Token.STIndicator)
  | Neg t -> (Token.toString Token.LPar)
             ^ (Token.toString (Token.Minus))
             ^ (string_of_term t)
             ^ (Token.toString Token.RPar)
  | Intersection (t1, t2) -> (Token.toString Token.LPar)
                             ^ (Token.toString Token.Plus)
                             ^ (string_of_term t1)
                             ^ (Token.toString Token.Plus)
                             ^ (string_of_term t2)
                             ^ (Token.toString Token.RPar)
  | Without (t1, t2) -> (Token.toString Token.LPar)
                             ^ (Token.toString Token.Plus)
                             ^ (string_of_term t1)
                             ^ (Token.toString Token.Minus)
                             ^ (string_of_term t2)
                             ^ (Token.toString Token.RPar)
  | Union (t1, t2) -> (Token.toString Token.LPar)
                             ^ (Token.toString Token.Minus)
                             ^ (string_of_term t1)
                             ^ (Token.toString Token.Plus)
                             ^ (string_of_term t2)
                             ^ (Token.toString Token.RPar)
  | Nor (t1, t2) -> (Token.toString Token.LPar)
                             ^ (Token.toString Token.Minus)
                             ^ (string_of_term t1)
                             ^ (Token.toString Token.Minus)
                             ^ (string_of_term t2)
                             ^ (Token.toString Token.RPar)

type quantity = Universal | Particular
type quality = Affirmative | Negative

type statement = Statement of { quan : quantity; qual : quality; sub : term; pred : term }
               | Neg of statement
let rec string_of_statement = function
  | Statement { quan; qual; sub; pred } -> 
    let first = match quan with Universal -> "-" | Particular -> "+" in
    let second = match qual with Affirmative -> "+" | Negative -> "-" in
    first ^ (string_of_term sub) ^ second ^ (string_of_term pred)
  | Neg statement -> (Token.toString Token.Minus)
                     ^ (Token.toString Token.LPar)
                     ^ string_of_statement statement
                     ^ (Token.toString Token.RPar)

type judgement = { statement : statement
                 ; refs : judgement list
                 ; rule : rule }
(* returns the indexOf of e in a list *)
let indexOf e ls =
  let rec loop ls n =
    match ls with
    | [] -> failwith "didn't work"
    | el :: _ when el = e -> n
    | _ :: ls -> loop ls (n+1)
  in
  loop ls 0

let swap a b ls =
  let aEl = List.nth ls a in
  let bEl = List.nth ls b in
  List.mapi (fun i el -> if i = a then bEl else if i = b then aEl else el) ls

let string_of_judgement judgement =
  let rec flatten jud =
    let flatRefs = List.map flatten jud.refs in
    let flat = jud :: (List.concat flatRefs) in
    List.rev flat
  in
  let flat = flatten judgement in
  let givensFirst j1 j2 =
    match (j1.rule, j2.rule) with
    | (Premise, Premise) -> 0
    | (Supposition, Supposition) -> 0
    | (Premise, _) -> -1
    | (_, Premise) -> +1
    | (Supposition, _) -> -1
    | (_, Supposition) -> +1
    | _ -> 0
  in
  let flat = List.stable_sort givensFirst flat in
  let rec sort cur flats =
    if cur = (List.length flats) - 1 then flats
    else
      let curJud = List.nth flats cur in
      if List.length curJud.refs = 0 then sort (cur+1) flats else
      let refPositions = List.map (fun ref -> indexOf ref flats) curJud.refs in
      let maxRef = List.fold_left (fun acc cur -> if cur > acc then cur else acc) (List.hd refPositions) refPositions in
      if maxRef < cur then sort (cur+1) flats
      else
        let flats' = swap cur maxRef flats in
        sort 0 flats'
  in
  let flat = sort 0 flat in
  let justStatements = List.map (fun jud -> jud.statement) flat in
  let addLineNumRefs jud =
    let refStatements = List.map (fun j -> j.statement) jud.refs in
    let refIndexes = List.map (fun ref -> 1 + indexOf ref justStatements) refStatements in
    (jud, refIndexes)
  in
  let withLineNumRefs = List.map addLineNumRefs flat in
  let makeLineString line (jud, refs) =
    let statement = string_of_statement jud.statement in
    let rule = string_of_rule jud.rule in
    let refs = List.fold_left (fun acc i -> acc ^ (string_of_int i) ^ ", ") "" refs in
    Printf.sprintf "%d. %s\t\t(%s%s)\n" (line+1) statement refs rule
  in
  let pieces = List.mapi makeLineString withLineNumRefs in
  List.fold_left (fun acc s -> acc ^ s) "" pieces
