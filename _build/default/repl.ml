let rec repl () =
  let rec loop s =
    try
      let line = read_line () in
      loop (s ^ line)
    with End_of_file -> s
  in
  let input = loop "" in
  let tokens = Tokenizer.tokenize input in
  let (premises, conclusion) = Parser.parse tokens in
  let valid = Verify.verify premises conclusion in
  let valid =
    match valid with
    | None -> "not valid"
    | Some derivation -> Derivation.toString derivation
  in
  let premises = Statement.toStrings premises in
  let conclusion = "/" ^ (Statement.toString conclusion) in
  let _ = Printf.printf "%s%s\n%s" premises conclusion valid in
  repl ()

let () = repl ()
