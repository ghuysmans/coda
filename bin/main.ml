open Coda

let dump = function (* TODO enable with -d *)
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
      (* TODO wrap errors in tokenize then write my own loop function? *)
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

(* TODO read multiple files *)
(* TODO extract archive entries (using libarchive bindings) *)
(* TODO generate a CSV with custom columns *)

let () =
  let fn, ledger =
    let show_usage () =
      Printf.eprintf "usage: %s [-f act.ledger] input.cod\n" Sys.argv.(0);
    in
    match Sys.argv with
    | [| _; "-h" |] | [| _; "--help" |] -> show_usage (); exit 0
    | [| _; fn |] -> fn, None
    | [| _; "-f"; ledger; fn |] -> fn, Some ledger
    | _ -> show_usage (); exit 1
  in
  let lookup, oc =
    let h = Hashtbl.create 100 in
    Hashtbl.mem h,
    match ledger with
    | Some fn ->
      let fd = Unix.openfile fn Unix.[O_RDWR; O_APPEND] 0o640 in
      (* On Windows, caml_unix_open moves the file pointer for no reason:
         https://github.com/ocaml/ocaml/blob/trunk/otherlibs/unix/open_win32.c#L99
         https://github.com/ocaml/ocaml/blob/trunk/otherlibs/unix/open_unix.c
         This is a portability issue. *)
      ignore (Unix.lseek fd 0 Unix.SEEK_SET); (* XXX before the upstream fix *)
      let ic = Unix.in_channel_of_descr fd in
      (* FIXME the manual is wrong: "On Windows: set_binary_mode_in always fails on channels created with [in_channel_of_descr]." *)
      set_binary_mode_in ic false;
      let re = Str.regexp {|;\([0-9A-Z]+\)|} in
      begin try
        while true do
          let l = input_line ic in
          try
            ignore (Str.search_forward re l 0);
            Hashtbl.replace h (Str.matched_group 1 l) ()
          with Not_found ->
            ()
        done
      with End_of_file ->
        Unix.out_channel_of_descr fd
      end
    | None ->
      stdout
  in
  let t =
    let pos = {Lexing.dummy_pos with pos_fname=fn} in
    let ic = open_in fn in
    Parser.MenhirInterpreter.loop
      (Lexer.supplier pos ic)
      (Parser.Incremental.statement pos)
  in
  match Ast.check t with
  | Ok t ->
    let once = ref true in
    Printf.eprintf "%d/%d/%d len=%d\n"
      t.header.creation.day t.header.creation.month t.header.creation.year
      (List.length t.transactions);
    List.iter (fun ((m, _, m''), _) ->
      if lookup m.Record.ref then
        Printf.eprintf "dup %S\n" m.ref
      else (
        Printf.eprintf "new %S\n" m.ref;
        if !once then (
          Printf.fprintf oc "\n;imported from %S\n" fn;
          once := false;
        );
        Printf.fprintf oc "%d/%02d/%02d  %s ;%s\n    A:Banque        %g€\n"
          m.value.year m.value.month m.value.day
          (Record.show_communication m.comm) m.ref
          (float m.amount /. -1000.);
        let name =
          match m'' with
          | Some {Record.name; _} -> name
          | _ -> "TODO"
        in
        if m.amount > 0 then
          Printf.fprintf oc "    Ch:%s\n" name
        else
          Printf.fprintf oc "    Pr:%s\n" name
      )
    ) t.transactions
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
  | Error `Unhandled_operation_type ->
    Printf.eprintf "error: unhandled operation type\n"
