module Date = struct
  type t = {
    day: int;
    month: int;
    year: int;
  }

  let parse s col =
    let ofs = col-1 in
    (* TODO validate *)
    {
      day = int_of_string (String.sub s ofs 2);
      month = int_of_string (String.sub s (ofs+2) 2);
      year = 2000 + int_of_string (String.sub s (ofs+4) 2);
    }
end

type header = { (** p. 15 *)
  creation: Date.t;
  dup: bool;
  bank: string;
  ref: string;
  dest: string;
  vat: string;
}

let parse_header l =
  assert (l.[127] = '2'); (* version *)
  let creation = Date.parse l 6 in
  assert (l.[14]='0' && l.[15]='5'); (* app *)
  let dup =
    match l.[16] with
    | ' ' -> false
    | 'D' -> true
    | _ -> invalid_arg "dup"
  in
  let bank = String.sub l 11 3 in
  let ref = String.sub l 24 10 in
  let dest = String.sub l 34 26 in
  let vat =
    let titu = String.sub l 71 11 in
    assert (titu.[0]='0');
    String.sub titu 1 10
  in
  {creation; dup; bank; ref; dest; vat}

type account = string (* TODO *)

type currency = string (* TODO *)

type old_balance = { (** p. 16 *)
  account: account;
  currency: currency;
  seq: int;
  debit: bool;
  amount: int;
  date: Date.t;
  name: string;
  label: string;
  seq': int;
}

let parse_bool msg c =
  match c with
  | '1' -> true
  | '0' -> false
  | _ -> invalid_arg msg

let parse_debit = parse_bool "debit"

let parse_old_balance l =
  assert (l.[1] = '2'); (* Belgian IBAN, TODO handle other types *)
  let seq = int_of_string (String.sub l 2 3) in
  let account = String.sub l 5 34 in
  let currency = String.sub l 39 3 in
  let debit = parse_debit l.[42] in
  let amount = int_of_string (String.sub l 43 15) in
  let date = Date.parse l 59 in
  let name = String.sub l 64 26 in
  let label = String.sub l 90 35 in
  let seq' = int_of_string (String.sub l 125 3) in
  {account; currency; seq; debit; amount; date; name; label; seq'}

module Operation = struct
  type typ = (** p. 5 *)
    | Simple
    | Customer
    | Customer_detail
    | Bank
    | Bank_detail
    | Bank_detail'
    | Bank_detail''
    | Detailed
    | Detail

  let typ_of_char = function
    | '0' -> Simple
    | '1' -> Customer
    | '5' -> Customer_detail
    | '2' -> Bank
    | '6' -> Bank_detail
    | '7' -> Bank_detail'
    | '9' -> Bank_detail''
    | '3' -> Detailed
    | '8' -> Detail
    | _ -> invalid_arg "Operation.typ_of_char"

  let char_of_typ = function
    | Simple -> '0'
    | Customer -> '1'
    | Customer_detail -> '5'
    | Bank -> '2'
    | Bank_detail -> '6'
    | Bank_detail' -> '7'
    | Bank_detail'' -> '9'
    | Detailed -> '3'
    | Detail -> '8'

  type family = (** p. 27 *)
    | Transfer (** SEPA *)
    | Instant_transfer
    | Check
    | Card
    | Direct_debit
    | Fee_or_interest
    (* TODO try counter *)
    | International (** non-SEPA *)
    | Foreign_check
    | Other of char * char

  let family_of_couple = function
    | '0', '1' -> Transfer
    | '0', '2' -> Instant_transfer
    | '0', '3' -> Check
    | '0', '4' -> Card
    | '0', '5' -> Direct_debit
    | '3', '5' -> Fee_or_interest
    | '4', '1' -> International
    | '4', '3' -> Foreign_check
    | a, b -> Other (a, b)

  let couple_of_family = function
    | Transfer -> '0', '1'
    | Instant_transfer -> '0', '2'
    | Check -> '0', '3'
    | Card -> '0', '4'
    | Direct_debit -> '0', '5'
    | Fee_or_interest -> '3', '5'
    | International -> '4', '1'
    | Foreign_check -> '4', '3'
    | Other (a, b) -> a, b

  type operation = Op of char * char (** p. 28-52 *)
  type rubric = R of char * char * char (** p. 53-58 *)

  type t = typ * family * operation * rubric (** p. 5 *)

  let parse l col =
    let ofs = col-1 in
    typ_of_char l.[ofs],
    family_of_couple (l.[ofs+1], l.[ofs+2]),
    Op (l.[ofs+3], l.[ofs+4]),
    R (l.[ofs+5], l.[ofs+6], l.[ofs+7])
end

type communication = (** p. 59 *)
  | Free of string
  | VCS of string
  | Structured of string * string (* TODO write a reassembly function *)

let parse_communication l col =
  let ofs = col-1 in
  if parse_bool "structured" l.[ofs] then
    let typ = String.sub l (ofs+1) 3 in
    let data = String.sub l (ofs+4) 50 in
    if typ = "101" then
      VCS (String.trim data)
    else
      Structured (typ, data)
  else
    Free String.(trim (sub l (ofs+1) 53))

let show_communication = function
  | Free s -> s
  | VCS s -> "+++" ^ s ^ "+++"
  | Structured (c, s) -> c ^ "#" ^ s

type movement = { (** p. 17 *)
  seq: int;
  detail: int;
  ref: string;
  debit: bool;
  amount: int;
  value: Date.t;
  op: Operation.t;
  comm: communication;
  posting: Date.t;
  seq': int;
  next_related_movement: bool;
  next_related_information: bool;
}

let parse_movement l =
  assert (l.[126]=' ');
  let seq = int_of_string (String.sub l 2 4) in
  let detail = int_of_string (String.sub l 6 4) in
  let ref = String.sub l 10 21 in
  let debit = parse_debit l.[31] in
  let amount = int_of_string (String.sub l 32 15) in
  let value = Date.parse l 48 in
  let op = Operation.parse l 54 in
  let comm = parse_communication l 62 in
  let posting = Date.parse l 116 in
  let seq' = int_of_string (String.sub l 121 3) in
  let next_related_movement = parse_bool "next_related_movement" l.[125] in
  let next_related_information = parse_bool "next_related_information" l.[127] in
  {seq; detail; ref; debit; amount; value; op; comm; posting; seq'; next_related_movement; next_related_information}

type r_typ = (** p. 19 *)
  | Reject
  | Return
  | Refund
  | Reversal
  | Cancellation

let r_typ_of_char = function
  | '1' -> Reject
  | '2' -> Return
  | '3' -> Refund
  | '4' -> Reversal
  | '5' -> Cancellation
  | _ -> invalid_arg "r_typ_of_char"

type movement' = { (** p. 19 *)
  seq: int;
  detail: int;
  comm': string;
  customer_ref: string;
  bic: string;
  r_typ: r_typ option;
  iso_return: string; (* TODO *)
  category_purpose: string; (** see https://www.iso20022.org/external_code_list.page *)
  purpose: string; (** see https://www.iso20022.org/external_code_list.page *)
  next_related_movement: bool;
  next_related_information: bool;
}

let parse_movement' l =
  assert (l.[109] = ' ');
  assert (l.[126] = ' ');
  let seq = int_of_string (String.sub l 2 4) in
  let detail = int_of_string (String.sub l 6 4) in
  let comm' = String.sub l 10 53 in
  let customer_ref = String.sub l 63 35 in
  let bic = String.sub l 98 11 in
  let r_typ =
    match l.[112] with
    | ' ' -> None
    | c -> Some (r_typ_of_char c)
  in
  let iso_return = String.sub l 113 4 in
  let category_purpose = String.sub l 117 4 in
  let purpose = String.sub l 121 4 in
  let next_related_movement = parse_bool "next_related_movement" l.[125] in
  let next_related_information = parse_bool "next_related_information" l.[127] in
  {seq; detail; comm'; customer_ref; bic; r_typ; iso_return; category_purpose; purpose; next_related_movement; next_related_information}

type movement'' = { (** p. 21 *)
  seq: int;
  detail: int;
  account: account;
  currency: currency;
  name: string;
  next_related_information: bool;
}

let parse_movement'' l =
  assert (l.[125] = '0');
  assert (l.[126] = ' ');
  let seq = int_of_string (String.sub l 2 4) in
  let detail = int_of_string (String.sub l 6 4) in
  let account = String.sub l 10 34 in
  let currency = String.sub l 44 3 in
  let name = String.sub l 47 35 in
  let next_related_information = parse_bool "next_related_information" l.[127] in
  {seq; detail; account; currency; name; next_related_information}

type generic_information = {
  article: char;
  seq: int;
  detail: int;
  (* TODO p. 21, 69 *)
  next_related_information: bool;
  next_unrelated_information: bool; (** TODO check *)
}

let parse_generic_information l =
  assert (l.[126] = ' ');
  let article = l.[1] in
  let seq = int_of_string (String.sub l 2 4) in
  let detail = int_of_string (String.sub l 6 4) in
  let next_related_information = parse_bool "next_related_information" l.[125] in
  let next_unrelated_information = parse_bool "next_unrelated_information" l.[127] in
  {article; seq; detail; next_related_information; next_unrelated_information}

type new_balance = { (** p. 24 *)
  seq: int;
  account: account;
  currency: currency;
  debit: bool;
  amount: int;
  date: Date.t;
  next_free: bool;
}

let parse_new_balance l =
  let seq = int_of_string (String.sub l 1 3) in
  let account = String.sub l 4 34 in
  let currency = String.sub l 38 3 in
  let debit = parse_debit l.[41] in
  let amount = int_of_string (String.sub l 42 15) in
  let date = Date.parse l 58 in
  let next_free = parse_bool "next_free" l.[127] in
  {seq; account; currency; debit; amount; date; next_free}

type footer = { (** p. 26 *)
  records: int; (** 1, 2.1, 2.2, 2.3, 3.1, 3.2, 3.3, 8 *)
  total_debit: int; (** type 2 with detail=0 *)
  total_credit: int; (** type 2 with detail=0 *)
  next_file: bool;
}

let parse_footer l =
  assert (l.[1]=' ' && l.[52]=' ');
  let records = int_of_string (String.sub l 16 6) in
  let total_debit = int_of_string (String.sub l 22 15) in
  let total_credit = int_of_string (String.sub l 37 15) in
  let next_file =
    match l.[127] with
    | '1' -> true
    | '2' -> false
    | _ -> invalid_arg "next_file"
  in
  {records; total_debit; total_credit; next_file}

type free = { (** p. 25 *)
  seq: int;
  detail: int;
  message: string;
  next_free: bool;
}

let parse_free l =
  assert (l.[1]=' ' && l.[10]=' ' && l.[112]=' ');
  let seq = int_of_string (String.sub l 2 4) in
  let detail = int_of_string (String.sub l 6 4) in
  let message = String.sub l 32 80 in
  let next_free = parse_bool "next_free" l.[127] in
  {seq; detail; message; next_free}


type t = (** p. 4 *)
  | Header of header
  | Old_balance of old_balance
  | Movement of movement
  | Movement' of movement'
  | Movement'' of movement''
  | Information of generic_information
  | Free of free
  | New_balance of new_balance
  | Footer of footer

let parse l =
  match l.[0] with
  | '0' -> Ok (Header (parse_header l))
  | '1' -> Ok (Old_balance (parse_old_balance l))
  | '8' -> Ok (New_balance (parse_new_balance l))
  | '9' -> Ok (Footer (parse_footer l))
  | '3' -> Ok (Information (parse_generic_information l))
  | '4' -> Ok (Free (parse_free l))
  | '2' ->
    begin match l.[1] with
    | '1' -> Ok (Movement (parse_movement l))
    | '2' -> Ok (Movement' (parse_movement' l))
    | '3' -> Ok (Movement'' (parse_movement'' l))
    | c -> Error (`Unknown_family c)
    end
  | c -> Error (`Unknown_record c)

let dump = function
  | Header {creation; dest; vat; _} ->
    Printf.printf "header: %d/%d/%d, dest %S, tva %S\n"
      creation.day creation.month creation.year dest vat
  | Old_balance {account; currency; seq; debit; amount; date; name; label; seq'} ->
    Printf.printf "old balance: %d/%d/%d#%d %d #%d\n"
      date.day date.month date.year seq amount seq';
  | New_balance {seq; debit; amount; date; _} ->
    Printf.printf "new balance: %d/%d/%d#%d %d\n"
      date.day date.month date.year seq (if debit then -amount else amount)
  | Footer {records; total_debit; total_credit; next_file} ->
    Printf.printf "footer: rec=%d d=%d c=%d %s\n"
      records total_debit total_credit
      (if next_file then "next" else "last")
    (* TODO compare counts and balances *)
  | Movement {seq; ref; debit; amount; value; op = typ, fam, _, _; comm; _} ->
    let opt = Operation.char_of_typ typ in
    let f1, f2 = Operation.couple_of_family fam in
    Printf.printf "movement: %d/%d/%d#%d %S T%cF%c%c %d€ %S\n"
      value.day value.month value.year seq ref opt f1 f2
      (if debit then -amount else amount) (show_communication comm)
  | Movement' {seq; comm'; bic; _} ->
    Printf.printf "movement': #%d ... %S BIC %s\n" seq comm' bic
  | Movement'' {seq; account; currency; name; _} ->
    Printf.printf "movement'': #%d %s in %S for %s\n"
      seq account currency name
  | Information {seq; _} ->
    Printf.printf "information: #%d\n" seq
  | Free {seq; message; _} ->
    Printf.printf "message: #%d %S\n" seq message


let () =
  try
    while true do
      match parse (read_line ()) with
      | Ok t -> dump t
      | Error (`Unknown_record c) ->
        Printf.eprintf "unknown record type %c\n" c;
        exit 1
      | Error (`Unknown_family c) ->
        Printf.eprintf "unknown family %c\n" c; (* FIXME *)
        exit 1
    done
  with End_of_file ->
    ()
