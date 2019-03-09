type t = 
  | LPar
  | RPar
  | Plus
  | Minus
  | Term of char (* elementary term *) (* any way to enfore capitalization in type? *)
  | STIndicator
  | ConcInd (* conclusion indicator *)

let toString token = 
  match token with
  | LPar -> "("
  | RPar -> ")"
  | Plus -> "+"
  | Minus -> "-"
  | Term c -> (Char.escaped c) 
  | STIndicator -> "*"
  | ConcInd -> "/"

let toStrings tokens =
  let tokens = List.map toString tokens in
  List.fold_left (fun acc s -> acc ^ s) "" tokens
