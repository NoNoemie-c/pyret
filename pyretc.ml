
let () = 
  open_in Sys.argv.(1) |>
  Lexing.from_channel |>
  Parser.file Lexer.read |>
  Ast.print_file