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
    | Some derivation -> Lang.string_of_derivation derivation
  in
  let premises = List.map Lang.string_of_statement premises in
  let premises = List.fold_left (fun acc s -> acc ^ s ^ "\n") "" premises in
  let conclusion = "/" ^ (Lang.string_of_statement conclusion) in
  let _ = Printf.printf "%s%s\n%s" premises conclusion valid in
  repl ()

let () = repl ()
