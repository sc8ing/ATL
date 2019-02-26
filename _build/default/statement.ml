type term = Term of char | Neg of term

type subPred = Plus of term | Minus of term

type t = Statement of { sub : subPred; pred : subPred } | Neg of t

let toString (Statement { sub = sub; pred = pred }) =
  let rec string_of_term t =
    match t with
    | Term c -> Char.escaped c
    | Neg t -> Printf.sprintf "-(%s)" (string_of_term t)
  in
  let string_of_subPred s =
    match s with
    | Plus e -> Printf.sprintf "+%s" (string_of_term e)
    | Minus e -> Printf.sprintf "-%s" (string_of_term e)
  in
  Printf.sprintf "%s%s" (string_of_subPred sub) (string_of_subPred pred)

let toStrings statements =
  List.fold_left (fun acc s -> acc ^ s ^ "\n") "" statements
