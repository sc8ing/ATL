open Lang

(* verify:
 *  if conclusion is in judgements, return judgement
 *  else
 *    let judgements' = applying all possible rules to every statement derived so far in
 *    let judgements' = { judgements } U { judgements' } in (without redundant statements) in
 *    verify judgements' conclusion
 *)
let verify premises conclusion =
  let rec verify judgements conclusion =
  in
  let jFromP p = Judgement { statement = p
                           ; refs = []
                           ; rule = Premise }
  in
  let premJudgements = List.map jFromP premises in
  verify premJudgements conclusion
