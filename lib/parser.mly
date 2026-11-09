%token <Record.header> HEADER
%token <Record.old_balance> OLD_BALANCE
%token <Record.movement> MOVEMENT
%token <Record.movement'> MOVEMENT2
%token <Record.movement''> MOVEMENT3
%token <Record.information> INFORMATION
%token <Record.information'> INFORMATION2
%token <Record.information''> INFORMATION3
%token <Record.free> FREE
%token <Record.new_balance> NEW_BALANCE
%token <Record.footer> FOOTER
%token EOF
%type <Ast.infos option> infos_or_epsilon
%type <Ast.transaction> transaction
%type <Ast.transaction list> transaction_list
%type <Record.free list> free_list
%type <Ast.statement list> statement
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
