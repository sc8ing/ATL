open Lang
open Token

let parseStatement tokens =
  match tokens with
  | LPar :: tokens ->
    let (innerS, tokens) = parseStatement tokens in
    begin
      match (innerS, tokens) with
      | (Some s, RPar :: []) -> Some s
      | _ -> None
    end
  | Minus :: tokens ->
    let (maybeS, statTokens) = parseStatement tokens in
    begin
      match (maybeS, statTokens) with
      | (Some s, []) -> Neg s
      | _ ->
        begin

        end
    end
  | Plus :: tokens ->
