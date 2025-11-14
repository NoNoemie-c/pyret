
let () = 
  open_in Sys.argv.(1) |>
  Lexing.from_channel |>
  Parser.file Lexer.token |>
  Ast.print_file