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
  let derivation = Verify.findDerivation premises conclusion in
  let derivation =
    match derivation with
    | None -> "no derivation found"
    | Some judgement ->
      String.make 30 '-' ^ "derivation" ^ String.make 30 '-' ^ "\n"
      ^ Lang.string_of_judgement judgement
  in
  let raa = Verify.findRAA premises conclusion in
  let raa =
    match raa with
    | None -> "no raa found"
    | Some (j1, j2) ->
      let j1 = Lang.string_of_judgement j1 in
      let j2 = Lang.string_of_judgement j2 in
      String.make 30 '-' ^ "RAA" ^ String.make 30 '-' ^ "\n"
      ^ Printf.sprintf "%s\ncontradicts\n\n%s" j1 j2
  in
  Printf.printf "%s" derivation ;
  Printf.printf "%s" raa ;
  repl ()

let () = repl ()
