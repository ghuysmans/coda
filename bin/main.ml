open Coda

let dump = function
  | Record.Header {creation; dest; vat; _} ->
    Printf.printf "header: %d/%d/%d, dest %S, tva %S\n"
      creation.day creation.month creation.year dest vat
  | Old_balance {seq; amount; date; seq'; _} ->
    Printf.printf "old balance: %d/%d/%d#%d %d #%d\n"
      date.day date.month date.year seq amount seq';
  | New_balance {seq; amount; date; _} ->
    Printf.printf "new balance: %d/%d/%d#%d %d\n"
      date.day date.month date.year seq amount
  | Footer {records; total_debit; total_credit; next_file} ->
    Printf.printf "footer: rec=%d d=%d c=%d %s\n"
      records total_debit total_credit
      (if next_file then "next" else "last")
  | Movement {seq; ref; amount; value; op = typ, fam, _, _; comm; _} ->
    let opt = Record.Operation.char_of_typ typ in
    let f1, f2 = Record.Operation.couple_of_family fam in
    Printf.printf "movement: %d/%d/%d#%d %S T%cF%c%c %d€ %S\n"
      value.day value.month value.year seq ref opt f1 f2
      amount (Record.show_communication comm)
  | Movement' {seq; comm'; bic; _} ->
    Printf.printf "movement': #%d ... %S BIC %s\n" seq comm' bic
  | Movement'' {seq; account; currency; name; _} ->
    Printf.printf "movement'': #%d %s in %S for %s\n"
      seq account currency name
  | (Information {seq; _} | Information' {seq; _} | Information'' {seq; _}) ->
    Printf.printf "information: #%d\n" seq
  | Free {seq; message; _} ->
    Printf.printf "message: #%d %S\n" seq message


(*
let () =
  try
    while true do
      match Record.parse (read_line ()) with
      | Ok t -> dump t
      | Error (`Unknown_record c) ->
        Printf.eprintf "unknown record type %c\n" c;
        exit 1
      | Error (`Unknown_family c) ->
        Printf.eprintf "unknown family %c\n" c; (* FIXME *)
        exit 1
      | Error (`Unknown_info c) ->
        Printf.eprintf "unknown info article %c\n" c;
        exit 1
      | Error `Invalid_length ->
        Printf.eprintf "invalid length\n";
        exit 2
    done
  with End_of_file ->
    ()
*)

let () =
  let t =
    let pos = {Lexing.dummy_pos with pos_fname="<stdin>"} in
    Parser.MenhirInterpreter.loop
      (Lexer.supplier pos stdin)
      (Parser.Incremental.statement pos)
  in
  match Ast.check t with
  | Ok t ->
    Printf.printf "%d/%d/%d len=%d count=%d calc=%d\n"
      t.header.creation.day t.header.creation.month t.header.creation.year
      (List.length t.transactions)
      t.footer.records
      (Ast.count_records t)
  | Error `Count ->
    Printf.eprintf "error: count mismatch\n"
  | Error `Creation_date ->
    Printf.eprintf "error: creation date mismatch\n"
  | Error `New_balance ->
    Printf.eprintf "error: balance mismatch\n"
  | Error `Total_credit ->
    Printf.eprintf "error: total credit mismatch\n"
  | Error `Total_debit ->
    Printf.eprintf "error: total debit mismatch\n"
  | Error `VAT ->
    Printf.eprintf "error: VAT number mismatch\n"
