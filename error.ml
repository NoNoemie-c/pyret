
exception Lexer of (unit -> unit)
exception Parser of Lexing.position * Lexing.position * (unit -> unit)
exception Typer of Lexing.position * Lexing.position * (unit -> unit)