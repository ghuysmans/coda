let tokenize f =
  match Record.parse (input_line f) with
  | exception End_of_file -> Parser.EOF
  | Error _ -> failwith "lexing error"
  | Ok x ->
    match x with
    | Header h -> Parser.HEADER h
    | Old_balance ob -> Parser.OLD_BALANCE ob
    | Movement m -> Parser.MOVEMENT m
    | Movement' m -> Parser.MOVEMENT2 m
    | Movement'' m -> Parser.MOVEMENT3 m
    | Information i -> Parser.INFORMATION i
    | Information' i -> Parser.INFORMATION2 i
    | Information'' i -> Parser.INFORMATION3 i
    | Free f -> Parser.FREE f
    | New_balance nb -> Parser.NEW_BALANCE nb
    | Footer x -> Parser.FOOTER x

let supplier init f =
  let line = ref 1 in
  fun () ->
    let tok = tokenize f in
    let st = !line * 129 in (* FIXME check line ending *)
    let pos = {init with Lexing.pos_lnum=(!line); pos_bol=st; pos_cnum=st} in
    let pos' = {pos with pos_cnum = st+128} in
    tok, pos, pos'
