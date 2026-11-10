open Record

type ('a, 'b, 'c) ott =
  | One of 'a
  | Two of 'a * 'b
  | Three of 'a * 'b * 'c

let count = function
  | One _ -> 1
  | Two _ -> 2
  | Three _ -> 3

type infos = (information, information', information'') ott
type transaction = (movement, movement', movement'') ott * infos option

let movement_of_transaction = function
  | (One m | Two (m, _) | Three (m, _, _)), _ -> m

type statement = {
  header: header;
  old_balance: old_balance;
  transactions: transaction list;
  new_balance: new_balance option;
  free_list: free list;
  footer: footer;
}

let count_records {transactions; new_balance; _} =
  1 (* old_balance *) +
  (if new_balance = None then 0 else 1) +
  List.fold_left (fun acc (m, io) ->
    acc +
    count m +
    match io with
    | None -> 0
    | Some i -> count i
  ) 0 transactions

let value txn =
  let m = movement_of_transaction txn in
  match m.op with
  | Record.Operation.Simple, _, _, _ -> Ok m.amount
  | _ -> Error `Unhandled_operation_type

let check ?dt ?vat ({header; old_balance; transactions; new_balance; footer; _} as stmt) =
  match dt with
  | Some dt when dt<>header.creation ->
    Error `Creation_date
  | _ ->
    match vat with
    | Some vat when vat<>header.vat ->
      Error `VAT
    | _ ->
      let (let*) = Result.bind in
      let* td, tc =
        List.fold_left (fun r x ->
          let* td, tc = r in
          let* amount = value x in
          if amount > 0 then Ok (td + amount, tc)
          else Ok (td, tc - amount) (* negative *)
        ) (Ok (0, 0)) transactions
      in
      if count_records stmt <> footer.records then
        Error `Count
      else
        if footer.total_debit <> td then
          Error `Total_debit
        else
          if footer.total_credit <> tc then
            Error `Total_credit
          else
            match new_balance with
            | None -> Ok stmt
            | Some b ->
              let nb = old_balance.amount + td - tc in
              if nb <> b.amount then
                Error `New_balance
              else
                Ok stmt
