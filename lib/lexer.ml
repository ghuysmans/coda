let tokenize f =
  match Record.parse (input_line f) with
  | exception End_of_file -> Record.EOF
  | Error _ -> failwith "lexing error"
  | Ok x -> x

let supplier init f =
  let line = ref 1 in
  fun () ->
    let tok = tokenize f in
    let st = !line * 129 in (* FIXME check line ending *)
    let pos = {init with Lexing.pos_lnum=(!line); pos_bol=st; pos_cnum=st} in
    let pos' = {pos with pos_cnum = st+128} in
    tok, pos, pos'
