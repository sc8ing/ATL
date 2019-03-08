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
  let valid = Verify.findDerivation premises conclusion in
  let valid =
    match valid with
    | None -> "not valid"
    | Some judgement -> Lang.string_of_judgement judgement
  in
  Printf.printf "%s" valid ;
  repl ()

let () = repl ()
