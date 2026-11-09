let tokenize f =
  match Record.parse (input_line f) with
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
