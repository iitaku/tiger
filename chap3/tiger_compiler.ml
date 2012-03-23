open Tiger_lexer

let main () =
  let lexbuf = Lexing.from_channel stdin in
  let tokens = List.rev (lexing_tiger lexbuf []) in
  Tiger_lexer.print_tokens tokens

let _ = Printexc.print main ()
