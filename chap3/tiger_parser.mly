%{
%}
%token EOF
%token TYPE VAR FUNCTION BREAK OF END IN NIL LET DO TO FOR WHILE ELSE IF ARRAY 
%token ASSIGN OR AND GE GT LE LT NEQ EQ DIVIDE TIMES MINUS PLUS 
%token COMMA DOT RBRACE LBRACE RBRACK LBRACK RPAREN LPAREN SEMICOLON COLON 
%token <string> STRING
%token <string> ID
%token <int> INT
%start prog
%type <unit> prog
%%

prog:
  expr EOF {}
;
expr:
  TYPE {}
;
%%

