{ }
rule translate = parse
  | "current_directory" { print_string (Sys.getcwd ()); translate lexbuf }
  | _ as c              { print_char c; translate lexbuf }
  | eof                 { exit 0 }
