let parseonly = ref false
let typeonly = ref false

let compile p = 
  try 
    open_in p |> 
    Lexing.from_channel |>
    Parser.file Lexer.token |> 
    if !parseonly then Ast.print_file 
    else (fun a -> Typer.check a |>
      if !typeonly then Tast.print_file else Producer.emit)
  with 
  | Parser.Error -> 
    Printf.eprintf "parsing error\n";
    exit 1
  | Lexer.Error -> 
    Printf.eprintf "lexing error\n";
    exit 1
  | Typer.Error ->
    Printf.eprintf "typing error\n";
    exit 1

let () =
  Arg.parse
    [
      "--parse-only", Arg.Set parseonly, "parse the source, without typing and
        compiling it";
      "--type-only", Arg.Set typeonly, "parse and type the source, without
        compiling it";
    ]
    compile
    ""