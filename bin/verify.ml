open Lang

let removeOptions optionals =
  let somesOnly = List.filter (function None -> false | _ -> true) optionals in
  List.map (function Some x -> x | _ -> failwith "error") somesOnly

(* stolen from https://stackoverflow.com/questions/3969321/lazy-n-choose-k-in-ocaml *)
let rec choose k l =
  if k = 0  then [ [] ]
  else
    let len = List.length l in
    if len < k then []
    else if k = len then [ l ]
    else
      match l with
      | [] -> assert false
      | h :: t ->
          let starting_with_h = List.map (fun sublist -> h :: sublist) (choose (pred k) t) in
          let not_starting_with_h = choose k t in
          starting_with_h @ not_starting_with_h



(* findDerivation:
 *  if conclusion is in judgements, return judgement
 *  else
 *    let judgements' = applying all possible rules to every statement derived so far in
 *    let judgements' = { judgements } U { judgements' } in (without redundant statements) in
 *    findDerivation judgements' conclusion
 *)
let findDerivation premises conclusion =
  let rec findDerivation judgements conclusion =
    let judEqualsConc = (fun j -> j.statement = conclusion) in
    let answer = List.filter judEqualsConc judgements in
    if List.length answer != 0 then Some (List.nth answer 0)
    else
      let unOpRules = [ PO; SO; Converse; Contrap; ADN; RDN; ST ] in
      let applySingle jud rule = Logic.applyIfEquivalent rule [jud] in
      let applyEachRule jud = removeOptions (List.map (applySingle jud) unOpRules) in
      let judsFromUnOps = List.concat (List.map applyEachRule judgements) in

      let binOpRules = [ DDO ] in
      let judPairs = choose 2 judgements in
      let applyDouble juds rule = Logic.applyIfEquivalent rule juds in
      let applyEachRule juds = removeOptions (List.map (applyDouble juds) binOpRules) in
      let judsFromBinOps = List.concat (List.map applyEachRule judPairs) in

      findDerivation (judsFromUnOps @ judsFromBinOps) conclusion
  in
  let jFromP p = { statement = p
                 ; refs = []
                 ; rule = Premise }
  in
  let premJudgements = List.map jFromP premises in
  findDerivation premJudgements conclusion


(* findRAA:
 *  Assume truth of all premises and the negation of the conclusion.
 *  If there's a contradiction, return the judgements that lead to it.
 *  Otherwise, apply all possible rules to the judgements and repeat with new judgements.
 *)
let findRAA premises conclusion =
  let rec findRAA judgements =
    let contradictions = List.map (fun j1 ->
        let s1 = j1.statement in
        let cont = List.find_opt (fun j2 -> Logic.contradicts s1 j2.statement) judgements in
        match cont with
        | None -> None
        | Some j2 -> Some (j1, j2)
      ) judgements
    in
    let contradictions = removeOptions contradictions in
    (* should these functions (findRAA and findDerivation) really return options? *)
    if List.length contradictions > 0 then Some (List.hd contradictions)
    else
      let unOpRules = [ PO; SO; Converse; Contrap; ADN; RDN; ST ] in
      let applySingle jud rule = Logic.applyIfEquivalent rule [jud] in
      let applyEachRule jud = removeOptions (List.map (applySingle jud) unOpRules) in
      let judsFromUnOps = List.concat (List.map applyEachRule judgements) in

      let binOpRules = [ DDO ] in
      let judPairs = choose 2 judgements in
      let applyDouble juds rule = Logic.applyIfEquivalent rule juds in
      let applyEachRule juds = removeOptions (List.map (applyDouble juds) binOpRules) in
      let judsFromBinOps = List.concat (List.map applyEachRule judPairs) in

      findRAA (judsFromUnOps @ judsFromBinOps)
  in
  let jFromP p = { statement = p
                 ; refs = []
                 ; rule = Premise }
  in
  let premJudgements = List.map jFromP premises in
  let assumption = { statement = Logic.negateStatement conclusion
                   ; refs = []
                   ; rule = Supposition }
  in
  let judgements = assumption :: premJudgements in
  findRAA judgements










