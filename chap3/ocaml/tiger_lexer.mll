{ 
  open Printf
  open Tiger_parser
  
  let comment_depth     = ref 0
}

let tiger_digit  = ['0'-'9']
let tiger_digit3 = tiger_digit tiger_digit tiger_digit
let tiger_escape = '\\'
let tiger_noescape = _ # tiger_escape
let tiger_id     = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule tiger = parse
  | [' '  '\t' '\n' ]   { tiger lexbuf }
  | "/*"                { incr comment_depth; tiger_comment lexbuf }
  | "type"              { TYPE       }
  | "var"               { VAR        }
  | "function"          { FUNCTION   }
  | "break"             { BREAK      }
  | "of"                { OF         }
  | "end"               { END        }
  | "in"                { IN         }
  | "nil"               { NIL        }
  | "let"               { LET        }
  | "do"                { DO         }
  | "to"                { TO         }
  | "for"               { FOR        }
  | "while"             { WHILE      }
  | "else"              { ELSE       }
  | "then"              { THEN       }
  | "if"                { IF         }
  | "array"             { ARRAY      }
  | ":="                { ASSIGN     }
  | "|"                 { OR         }
  | "&"                 { AND        }
  | ">="                { GE         }
  | ">"                 { GT         }
  | "<="                { LE         }
  | "<"                 { LT         }
  | "<>"                { NEQ        }
  | "="                 { EQ         }
  | "/"                 { DIVIDE     }
  | "*"                 { TIMES      }
  | "-"                 { MINUS      }
  | "+"                 { PLUS       }
  | "."                 { DOT        }
  | "}"                 { RBRACE     }
  | "{"                 { LBRACE     }
  | "]"                 { RBRACK     }
  | "["                 { LBRACK     }
  | ")"                 { RPAREN     }
  | "("                 { LPAREN     }
  | ";"                 { SEMICOLON  }
  | ":"                 { COLON      }
  | ","                 { COMMA      }
  | "\""                { tiger_string "" lexbuf }
  | tiger_digit+ as num { INT (int_of_string num) }
  | tiger_id     as id  { ID  (id) }
  | _ as c              { printf "unrecognized character : %c%!" c; tiger lexbuf }
  | eof                 { EOF }
and tiger_comment = parse
  | '\n'                { tiger_comment lexbuf }
  | "/*"                { incr comment_depth; tiger_comment lexbuf } 
  | "*/"                { decr comment_depth; match !comment_depth with 0 -> tiger lexbuf | _ -> tiger_comment lexbuf } 
  | _                   { tiger_comment lexbuf }
and tiger_string str = parse
  |  "\""                                       { STRING str }
  | (tiger_escape 'n')                  as code { tiger_string (str ^ code) lexbuf }
  | (tiger_escape 't')                  as code { tiger_string (str ^ code) lexbuf }
  | (tiger_escape '^' _)                as code { tiger_string (str ^ code) lexbuf }
  | (tiger_escape tiger_digit3)         as code { tiger_string (str ^ code) lexbuf }
  | (tiger_escape '"')                  as code { tiger_string (str ^ code) lexbuf }
  | (tiger_escape '\\')                 as code { tiger_string (str ^ code) lexbuf }
  | (tiger_escape tiger_noescape* tiger_escape) { tiger_string  str         lexbuf }
  | _                                   as c    { tiger_string (str ^ (Char.escaped c)) lexbuf }
{ 
    let rec lexing_tiger lexbuf tokens = 
        let token = tiger lexbuf in
        match token with
          | EOF -> token::tokens
          | _   -> lexing_tiger lexbuf (token::tokens)

    let rec print_tokens tokens = 
      let print_token = function
        | TYPE       -> printf "TYPE, "
        | VAR        -> printf "VAR, "
        | FUNCTION   -> printf "FUNCTION, "
        | BREAK      -> printf "BREAK, "
        | OF         -> printf "OF, "
        | END        -> printf "END, "
        | IN         -> printf "IN, "
        | NIL        -> printf "NIL, "
        | LET        -> printf "LET, "
        | DO         -> printf "DO, "
        | TO         -> printf "TO, "
        | FOR        -> printf "FOR, "
        | WHILE      -> printf "WHILE, "
        | ELSE       -> printf "ELSE, "
        | THEN       -> printf "THEN, "
        | IF         -> printf "IF, "
        | ARRAY      -> printf "ARRAY, "
        | ASSIGN     -> printf "ASSIGN, "
        | OR         -> printf "OR, "
        | AND        -> printf "AND, "
        | GE         -> printf "GE, "
        | GT         -> printf "GT, "
        | LE         -> printf "LE, "
        | LT         -> printf "LT, "
        | NEQ        -> printf "NEQ, "
        | EQ         -> printf "EQ, "
        | DIVIDE     -> printf "DIVIDE, "
        | TIMES      -> printf "TIMES, "
        | MINUS      -> printf "MINUS, "
        | PLUS       -> printf "PLUS, "
        | DOT        -> printf "DOT, "
        | RBRACE     -> printf "RBRACE, "
        | LBRACE     -> printf "LBRACE, "
        | RBRACK     -> printf "RBRACK, "
        | LBRACK     -> printf "LBRACK, "
        | RPAREN     -> printf "RPAREN, "
        | LPAREN     -> printf "LPAREN, "
        | SEMICOLON  -> printf "SEMICOLON, "
        | COLON      -> printf "COLON, "
        | COMMA      -> printf "COMMA, "
        | STRING str -> printf "STRING(%s), " str
        | INT    num -> printf "INT(%d), "    num 
        | ID     id  -> printf "ID(%s), "     id 
        | EOF        -> printf "EOF, " in
      match tokens with
        | [] -> ()
        | _  -> print_token (List.hd tokens); print_tokens (List.tl tokens)
    
    let main () =
      let lexbuf = Lexing.from_channel stdin in
      let tokens = List.rev (lexing_tiger lexbuf []) in
      print_tokens tokens

    let _ = Printexc.print main ()
}
