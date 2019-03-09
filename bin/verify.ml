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
      let unOpRules = [ PO; SO; Converse; Contrap; ADN; RDN ] in
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


