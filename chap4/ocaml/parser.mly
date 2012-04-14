%{
  open Printf
  open Semant

  let rec parse_error s = print_endline s
%}
%token EOF
%token TYPE VAR FUNCTION BREAK OF END IN NIL LET DO TO FOR WHILE ELSE THEN IF ARRAY 
%token ASSIGN OR AND GE GT LE LT NEQ EQ DIVIDE TIMES MINUS PLUS 
%token COMMA DOT RBRACE LBRACE RBRACK LBRACK RPAREN LPAREN SEMICOLON COLON 
%token <string> STRING
%token <string> ID
%token <int> INT
%nonassoc OF
%nonassoc DO
%nonassoc THEN 
%nonassoc ELSE
%nonassoc IF
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%start prog
%type <Semant.expr> prog
%%

prog:
  expr EOF { $1 }
decs:
  | dec      { [$1] }
  | dec decs {  $1 :: $2 }
dec:
  | tydec  { $1 }
  | vardec { $1 }
  | fundec { $1 }
tydec:
  | TYPE ID EQ ty { Semant.TyDec {symbol=$2; ty=ty; pos=0 }  }
ty:
  | ID                     { printf "ty(id)\n%!" }
  | LBRACE tyfields RBRACE { printf "ty\n%!" }
  | ARRAY OF ID            { printf "ty(array)\n%!" }
tyfields:
  | epsilon                    { printf "tyfields(epsilon)\n%!"}
  | ID COLON ID                { printf "tyfileds\n%!" }
  | ID COLON ID COMMA tyfields { printf "tyfileds\n%!" }
vardec:
  | VAR ID ASSIGN expr          { printf "vardec(ASSIGN)\n%!" }
  | VAR ID COLON ID ASSIGN expr { printf "vardec(COLON ASSIGN)\n%!" }
fundec:
  | FUNCTION ID LPAREN tyfields RPAREN EQ expr { printf "fundec(%s)\n%!" $2 }
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ expr { printf "fundec(%s)\n%!" $2 }
expr: 
  | ID                                 { printf "expr(ID %s)\n%!" $1 }
  | lvalue                             { printf "expr(lvalue)\n%! "}
  | NIL                                { printf "expr(NIL)\n%!" }
  | LPAREN RPAREN                      { printf "expr(())\n%!" }
  | INT                                { printf "expr(INT %d)\n%!" $1 }
  | STRING                             { printf "expr(STRING %s)\n%!" $1 }
  | MINUS expr                         { printf "expr(UMINUS)\n%!" }
  | expr PLUS expr                     { printf "expr(PLUS)\n%!" }
  | expr MINUS expr                    { printf "expr(MINUS)\n%!" }
  | expr TIMES expr                    { printf "expr(TIMES)\n%!" }
  | expr DIVIDE expr                   { printf "expr(DIVIDE)\n%!" }
  | expr EQ expr                       { printf "expr(EQ)\n%!" }
  | expr NEQ expr                      { printf "expr(NEQ)\n%!" }
  | expr LT expr                       { printf "expr(LT)\n%!" }
  | expr LE expr                       { printf "expr(LE)\n%!" }
  | expr GT expr                       { printf "expr(GT)\n%!" }
  | expr GE expr                       { printf "expr(GE)\n%!" }
  | expr AND expr                      { printf "expr(AND)\n%!" }
  | expr OR expr                       { printf "expr(OR)\n%!" }
  | expr ASSIGN expr                   { printf "expr(ASSIGN)\n%!" }
  | LPAREN expseq RPAREN               { printf "expr(expseq)\n%!" }
  | ID LBRACE record RBRACE            { printf "expr(record)\n%!" }
  | ID LPAREN args RPAREN              { printf "expr(%s())\n%!" $1}
  | ID LBRACK expr RBRACK OF expr      { printf "expr(expr)\n%!" }
  | IF expr THEN expr                  { printf "expr(IF THEN)\n%!" }
  | IF expr THEN expr ELSE expr        { printf "expr(IF THEN ELSE)\n%!" }
  | WHILE expr DO expr                 { printf "expr(WHILE)\n%!" }
  | FOR ID ASSIGN expr TO expr DO expr { printf "expr(FOR)\n%!" }
  | BREAK                              { printf "expr(BREAK)\n%!" }
  | LET decs IN expseq END             { printf "expr(let)\n%!" }
lvalue:
  | ID DOT ID                          { printf "lvalue(DOT ID)\n%!" }
  | ID LBRACK expr RBRACK              { printf "lvalue(LBACK expr RBRACK)\n%!" }
  | lvalue DOT ID                          { printf "lvalue(DOT ID)\n%!" }
  | lvalue LBRACK expr RBRACK              { printf "lvalue(LBACK expr RBRACK)\n%!" }
args:
  | epsilon         { printf "args(epsilon)\n%!" }
  | expr            { printf "args(expr)\n%!" }
  | expr COMMA args { printf "args(expr, args)\n%!" }
record:
  | epsilon                 { printf "record\n%!"}
  | ID EQ expr              { printf "record\n%!" }
  | ID EQ expr COMMA record { printf "record\n%!" }
expseq:
  | expr                  { printf "expseq\n%!" }
  | expr SEMICOLON expseq { printf "expseq\n%!" }
epsilon:                  { printf "epsilon\n%!" }
;
%%

