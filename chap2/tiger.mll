{ 
    open Printf

    type pos_t = int * int * int

    type token_t = TYPE      of pos_t option
                 | VAR       of pos_t option
                 | FUNCTION  of pos_t option
                 | BREAK     of pos_t option
                 | OF        of pos_t option
                 | END       of pos_t option
                 | IN        of pos_t option
                 | NIL       of pos_t option
                 | LET       of pos_t option
                 | DO        of pos_t option
                 | TO        of pos_t option
                 | FOR       of pos_t option
                 | WHILE     of pos_t option
                 | ELSE      of pos_t option
                 | IF        of pos_t option
                 | ARRAY     of pos_t option
                 | ASSIGN    of pos_t option
                 | OR        of pos_t option
                 | AND       of pos_t option
                 | GE        of pos_t option
                 | GT        of pos_t option
                 | LE        of pos_t option
                 | LT        of pos_t option
                 | NEQ       of pos_t option
                 | EQ        of pos_t option
                 | DIVIDE    of pos_t option
                 | TIMES     of pos_t option
                 | MINUS     of pos_t option
                 | PLUS      of pos_t option
                 | DOT       of pos_t option
                 | RBRACE    of pos_t option
                 | LBRACE    of pos_t option
                 | RBRACK    of pos_t option
                 | LBRACK    of pos_t option
                 | RPAREN    of pos_t option
                 | LPAREN    of pos_t option
                 | SEMICOLON of pos_t option
                 | COLON     of pos_t option
                 | COMMA     of pos_t option
                 | STRING    of pos_t option * string
                 | INT       of pos_t option * int
                 | ID        of pos_t option * string
                 | EOF
      

    let number_of_lines   = ref 0
    let begin_of_line_pos = ref 0 
    let comment_depth     = ref 0

    let create_pos lexbuf = 
      let pos = (Lexing.lexeme_start lexbuf) - !begin_of_line_pos in
      let len = (Lexing.lexeme_end   lexbuf) - !begin_of_line_pos in 
      Some (!number_of_lines, pos, len)
}

let tiger_string = '"' _* '"'
let tiger_digit  = ['0'-'9']
let tiger_id     = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule tiger = parse
  | [' '  '\t']         { tiger lexbuf }
  | '\n'                { incr number_of_lines; begin_of_line_pos := Lexing.lexeme_end lexbuf; tiger lexbuf }
  | "/*"                { incr comment_depth; tiger_comment lexbuf }
  | "type"              { TYPE      (create_pos lexbuf) }
  | "var"               { VAR       (create_pos lexbuf) }
  | "function"          { FUNCTION  (create_pos lexbuf) }
  | "break"             { BREAK     (create_pos lexbuf) }
  | "of"                { OF        (create_pos lexbuf) }
  | "end"               { END       (create_pos lexbuf) }
  | "in"                { IN        (create_pos lexbuf) }
  | "nil"               { NIL       (create_pos lexbuf) }
  | "let"               { LET       (create_pos lexbuf) }
  | "do"                { DO        (create_pos lexbuf) }
  | "to"                { TO        (create_pos lexbuf) }
  | "for"               { FOR       (create_pos lexbuf) }
  | "while"             { WHILE     (create_pos lexbuf) }
  | "else"              { ELSE      (create_pos lexbuf) }
  | "if"                { IF        (create_pos lexbuf) }
  | "array"             { ARRAY     (create_pos lexbuf) }
  | ":="                { ASSIGN    (create_pos lexbuf) }
  | "|"                 { OR        (create_pos lexbuf) }
  | "&"                 { AND       (create_pos lexbuf) }
  | ">="                { GE        (create_pos lexbuf) }
  | ">"                 { GT        (create_pos lexbuf) }
  | "<="                { LE        (create_pos lexbuf) }
  | "<"                 { LT        (create_pos lexbuf) }
  | "<>"                { NEQ       (create_pos lexbuf) }
  | "="                 { EQ        (create_pos lexbuf) }
  | "/"                 { DIVIDE    (create_pos lexbuf) }
  | "*"                 { TIMES     (create_pos lexbuf) }
  | "-"                 { MINUS     (create_pos lexbuf) }
  | "+"                 { PLUS      (create_pos lexbuf) }
  | "."                 { DOT       (create_pos lexbuf) }
  | "}"                 { RBRACE    (create_pos lexbuf) }
  | "{"                 { LBRACE    (create_pos lexbuf) }
  | "]"                 { RBRACK    (create_pos lexbuf) }
  | "["                 { LBRACK    (create_pos lexbuf) }
  | ")"                 { RPAREN    (create_pos lexbuf) }
  | "("                 { LPAREN    (create_pos lexbuf) }
  | ";"                 { SEMICOLON (create_pos lexbuf) }
  | ":"                 { COLON     (create_pos lexbuf) }
  | ","                 { COMMA     (create_pos lexbuf) }
  | tiger_string as str { STRING   ((create_pos lexbuf), str) }
  | tiger_digit+ as num { INT      ((create_pos lexbuf), int_of_string num) }
  | tiger_id     as id  { ID       ((create_pos lexbuf), id) }
  | _ as c              { printf "unrecognized character : %c" c; tiger lexbuf }
  | eof                 { EOF }
and tiger_comment = parse
  | '\n'                { incr number_of_lines; begin_of_line_pos := Lexing.lexeme_end lexbuf; tiger_comment lexbuf }
  | "/*"                { incr comment_depth; tiger_comment lexbuf } 
  | "*/"                { decr comment_depth; match !comment_depth with 0 -> tiger lexbuf | _ -> tiger_comment lexbuf } 
  | _                   { tiger_comment lexbuf }

{ 
    let rec lexing_tiger lexbuf tokens = 
        let token = tiger lexbuf in
        match token with
          | EOF -> token::tokens
          | _   -> lexing_tiger lexbuf (token::tokens)

    let rec print_tokens tokens = 
      let print_token = function
        | TYPE      pos       -> printf "TYPE, "
        | VAR       pos       -> printf "VAR, "
        | FUNCTION  pos       -> printf "FUNCTION, "
        | BREAK     pos       -> printf "BREAK, "
        | OF        pos       -> printf "OF, "
        | END       pos       -> printf "END, "
        | IN        pos       -> printf "IN, "
        | NIL       pos       -> printf "NIL, "
        | LET       pos       -> printf "LET, "
        | DO        pos       -> printf "DO, "
        | TO        pos       -> printf "TO, "
        | FOR       pos       -> printf "FOR, "
        | WHILE     pos       -> printf "WHILE, "
        | ELSE      pos       -> printf "ELSE, "
        | IF        pos       -> printf "IF, "
        | ARRAY     pos       -> printf "ARRAY, "
        | ASSIGN    pos       -> printf "ASSIGN, "
        | OR        pos       -> printf "OR, "
        | AND       pos       -> printf "AND, "
        | GE        pos       -> printf "GE, "
        | GT        pos       -> printf "GT, "
        | LE        pos       -> printf "LE, "
        | LT        pos       -> printf "LT, "
        | NEQ       pos       -> printf "NEQ, "
        | EQ        pos       -> printf "EQ, "
        | DIVIDE    pos       -> printf "DIVIDE, "
        | TIMES     pos       -> printf "TIMES, "
        | MINUS     pos       -> printf "MINUS, "
        | PLUS      pos       -> printf "PLUS, "
        | DOT       pos       -> printf "DOT, "
        | RBRACE    pos       -> printf "RBRACE, "
        | LBRACE    pos       -> printf "LBRACE, "
        | RBRACK    pos       -> printf "RBRACK, "
        | LBRACK    pos       -> printf "LBRACK, "
        | RPAREN    pos       -> printf "RPAREN, "
        | LPAREN    pos       -> printf "LPAREN, "
        | SEMICOLON pos       -> printf "SEMICOLON, "
        | COLON     pos       -> printf "COLON, "
        | COMMA     pos       -> printf "COMMA, "
        | STRING   (pos, str) -> printf "STRING(%s), " str
        | INT      (pos, num) -> printf "INT(%d), "    num 
        | ID       (pos, id)  -> printf "ID(%s), "     id 
        | EOF                 -> printf "EOF, " in
      match tokens with
        | [] -> ()
        | _  -> print_token (List.hd tokens); print_tokens (List.tl tokens)
    
    let main () =
      let lexbuf = Lexing.from_channel stdin in
      let tokens = List.rev (lexing_tiger lexbuf []) in
      print_tokens tokens

    let _ = Printexc.print main ()
}
