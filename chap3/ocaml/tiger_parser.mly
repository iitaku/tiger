%{
  type ast_t = Node of token * ast_t * ast_t | Leaf of token
  
  open Printf
%}
%token EOF
%token TYPE VAR FUNCTION BREAK OF END IN NIL LET DO TO FOR WHILE ELSE THEN IF ARRAY 
%token ASSIGN OR AND GE GT LE LT NEQ EQ DIVIDE TIMES MINUS PLUS 
%token COMMA DOT RBRACE LBRACE RBRACK LBRACK RPAREN LPAREN SEMICOLON COLON 
%token <string> STRING
%token <string> ID
%token <int> INT
%left THEN ELSE DO
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%start prog
%type <unit> prog
%%

prog:
  decs EOF {}
;
decs:
  | dec decs { printf "dec" }
;
dec:
  | tydec  { printf "tydec" }
  | vardec { printf "vardec" }
  | fundec { printf "fundec" }
;
tydec:
  | TYPE ID EQ ty { printf "tydec %s = %s" $2 $4 }
;
ty:
  | ID                     { printf "ID(%s)" $1 }
  | LBRACE tyfields RBRACE { printf "tyfield" }
  | ARRAY OF ID            { printf "array(%s)" $3 }
;
tyfields:
  | epsilon              { printf "epsilon" }
  | ID COLON ID tyfields { printf "id(%s) : id(%s)" $1 $3 }
vardec:
  | VAR ID ASSIGN expr          {}
  | VAR ID COLON ID ASSIGN expr {}
;
fundec:
  | FUNCTION ID tyfields          {}
  | FUNCTION ID tyfields COLON ID {}
;
expr: 
  | NIL {}
  | LPAREN RPAREN {}
  | LPAREN expseq RPAREN {}
  | INT {}
  | STRING {}
  | MINUS expr {}
  | expr PLUS expr {}
  | expr MINUS expr {}
  | expr TIMES expr {}
  | expr DIVIDE expr {}
  | expr EQ expr {}
  | expr NEQ expr {}
  | expr LT expr {}
  | expr LE expr {}
  | expr GT expr {}
  | expr GE expr {}
  | expr AND expr {}
  | expr OR expr {}
  | expr ASSIGN expr {}
  | ID LBRACE record RBRACE {}
  | ID LPAREN args RPAREN {}
  | ID LBRACK expr RBRACK {}
  | IF expr THEN {}
  | IF expr THEN expr ELSE expr {}
  | WHILE expr DO expr {}
  | FOR ID ASSIGN expr TO expr DO expr {}
  | BREAK {}
  | LET decs IN expseq END {}
;
args:
  | expr {}
  | expr COMMA args {}
record:
  | ID EQ expr {}
  | ID EQ expr COMMA record {}
expseq:
  | expr {}
  | expr SEMICOLON expseq {}
epsilon: {}
;
%%

