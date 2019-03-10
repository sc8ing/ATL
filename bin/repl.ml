let rec repl () =
  let rec loop premises =
    try
      let line = read_line () in
      let tokens = Tokenizer.tokenize line in
      match tokens with
      | Token.ConcInd :: tokens ->
        let conclusion = Parser.parse tokens in
        (premises, conclusion)
      | tokens -> loop ((Parser.parse tokens) :: premises)
    with End_of_file -> failwith "end of file"
  in
  let (premises, conclusion) = loop [] in
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
  Printf.printf "\n%s" derivation ;
  Printf.printf "\n%s" raa ;
  repl ()

let () = repl ()
