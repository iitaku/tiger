open Tiger_lexer
open Tiger_parser

let only_lexing () =
  let lexbuf = Lexing.from_channel stdin in
  let tokens = List.rev (lexing_tiger lexbuf []) in
  print_tokens tokens

let lexing_parsing () =
  let lexbuf = Lexing.from_channel stdin in
  let () = Printf.printf "compiling...\n%!" in
  let () = prog tiger lexbuf in
  Printf.printf "\ncompiled\n%!"

let _ = Printexc.print lexing_parsing ()
