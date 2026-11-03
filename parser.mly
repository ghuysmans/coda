%token <Coda.header> HEADER
%token <Coda.old_balance> OLD_BALANCE
%token <Coda.movement> MOVEMENT
%token <Coda.movement'> MOVEMENT2
%token <Coda.movement''> MOVEMENT3
%token <Coda.information> INFORMATION
%token <Coda.information'> INFORMATION2
%token <Coda.information''> INFORMATION3
%token <Coda.free> FREE
%token <Coda.new_balance> NEW_BALANCE
%token <Coda.footer> FOOTER
%token EOF
%type <unit> infos_or_epsilon transaction transaction_list free_list statement
%start statement
%%

infos_or_epsilon:
| { () }
| INFORMATION { () }
| INFORMATION INFORMATION2 { () }
| INFORMATION INFORMATION2 INFORMATION3 { () }
transaction:
| MOVEMENT infos_or_epsilon { () }
| MOVEMENT MOVEMENT2 infos_or_epsilon { () }
| MOVEMENT MOVEMENT2 MOVEMENT3 infos_or_epsilon { () }
transaction_list:
| { () }
| transaction transaction_list { () }
free_list:
| { () }
| FREE free_list { () }
statement:
| HEADER OLD_BALANCE transaction_list NEW_BALANCE free_list FOOTER EOF { () }
| HEADER OLD_BALANCE FOOTER EOF { () (* "empty" *) }
