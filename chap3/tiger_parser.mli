type token =
  | EOF
  | TYPE
  | VAR
  | FUNCTION
  | BREAK
  | OF
  | END
  | IN
  | NIL
  | LET
  | DO
  | TO
  | FOR
  | WHILE
  | ELSE
  | IF
  | ARRAY
  | ASSIGN
  | OR
  | AND
  | GE
  | GT
  | LE
  | LT
  | NEQ
  | EQ
  | DIVIDE
  | TIMES
  | MINUS
  | PLUS
  | COMMA
  | DOT
  | RBRACE
  | LBRACE
  | RBRACK
  | LBRACK
  | RPAREN
  | LPAREN
  | SEMICOLON
  | COLON
  | STRING of (string)
  | ID of (string)
  | INT of (int)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
