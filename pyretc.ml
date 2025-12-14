let parseonly = ref false
let typeonly = ref false


let compile p =  
  let lb = Lexing.from_channel (open_in p) in
  try
    Parser.file Lexer.token lb |>
    if !parseonly then Ast.print_file 
    else (fun a -> Typer.w a |>
      if !typeonly then Tast.print_file else Producer.emit)
  with 
  | Error.Lexer f -> 
    let sp = Lexing.lexeme_start_p lb in
    let ep = Lexing.lexeme_end_p lb in
    Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n"
      p sp.pos_lnum sp.pos_cnum ep.pos_cnum;
    f ();
    exit 1
  | Parser.Error -> (* the default menhir error *)
    Printf.eprintf "File \"%s\", line -1, characters -1--1:\nparsing error\n" p;
    exit 1
  | Error.Parser (sp, ep, f) -> 
    Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n"
      p sp.pos_lnum sp.pos_cnum ep.pos_cnum;
    f ();
    exit 1
  | Error.Typer (sp, ep, f) ->
    Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n"
      p sp.pos_lnum sp.pos_cnum ep.pos_cnum;
    f ();
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