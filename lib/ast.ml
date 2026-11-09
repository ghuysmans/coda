open Record

type ('a, 'b, 'c) ott =
  | One of 'a
  | Two of 'a * 'b
  | Three of 'a * 'b * 'c

type infos = (information, information', information'') ott
type transaction = (movement, movement', movement'') ott * infos option
type statement = {
  header: header;
  old_balance: old_balance;
  transactions: transaction list;
  new_balance: new_balance option;
  free_list: free list;
  footer: footer;
}
