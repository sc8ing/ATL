open Token

let isUpperCaseChar c =
  let code = Char.code c in
  code >= Char.code 'A' && code <= Char.code 'Z'

let explode s =
  let n = String.length s in
  let rec repeat i =
    match i = n with
    | true  -> []
    | false -> s.[i] :: repeat (i + 1)
  in
  repeat 0

(*let implode chars =
  let res = Bytes.create (List.length chars) in
  let rec repeat i chars =
    match chars with
    | [] -> res
    | char :: chars ->
       Bytes.set res i char;
       repeat (i + 1) chars
  in
  Bytes.to_string (repeat 0 chars)*)

let tokenize s =
  let tokenize chars =
    let rec loop tokens chars =
      match chars with
      | [] -> tokens
      | ' ' :: chars | '\n' :: chars -> loop tokens chars
      | '(' :: chars -> loop (LPar :: tokens) chars
      | ')' :: chars -> loop (RPar :: tokens) chars
      | '+' :: chars -> loop (Plus :: tokens) chars
      | '-' :: chars -> loop (Minus :: tokens) chars
      | '/' :: chars -> loop (ConcInd :: tokens) chars
      | c :: chars ->
        if isUpperCaseChar c then loop (Term c :: tokens) chars
        else failwith (Printf.sprintf "invalid character %c" c)
    in
    List.rev (loop [] chars)
  in
  tokenize (explode s)
