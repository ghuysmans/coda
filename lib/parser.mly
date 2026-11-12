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
%type <Ast.infos> infos
%type <Ast.transaction> transaction
%type <Ast.statement> statement
%start statement
%%

infos:
| i=INFORMATION { Ast.One i }
| i=INFORMATION j=INFORMATION2 { Ast.Two (i, j) }
| i=INFORMATION j=INFORMATION2 k=INFORMATION3 { Ast.Three (i, j, k) }
transaction:
| m=MOVEMENT n=MOVEMENT2? o=MOVEMENT3? i=infos? { (m, n, o), i }
statement:
| h=HEADER o=OLD_BALANCE t=transaction* n=NEW_BALANCE fr=FREE* ft=FOOTER EOF {
  {Ast.header=h; old_balance=o; transactions=t; new_balance=Some n; footer=ft;
   free_list=fr}
}
| h=HEADER o=OLD_BALANCE ft=FOOTER EOF { (* "empty" *)
  {Ast.header=h; old_balance=o; transactions=[]; new_balance=None; footer=ft;
   free_list=[]}
}
