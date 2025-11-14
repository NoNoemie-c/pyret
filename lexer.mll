


(** Analyse syntaxique élémentaire d'expressions arithmétiques
    formées de constantes, addition, multiplication et parenthèses. *)

{
  open Lexing
  open Ast
  open Parser
}

let linecomment = "#" [^'\n'] "\n" 
let whitespace = [' ' '\t' '\n']+
let digit     = ['0'-'9']
let integer = ("+" | "-")? digit+
let letter = ['a'-'z' 'A'-'Z' '_']
let escape = "\\\\" | "\\'" | "\\\"" | "\\t" | "\\n"
let string = "\"" ([^'"' '\n'] | escape)* "\"" 
| "\'" ([^'\'' '\n'] | escape)* "\'"
let ident = letter ("-"* (letter | digit)+)*

rule token = parse
| whitespace | linecomment { token lexbuf }

| "false" { CONST (CBool false) }
| "true" { CONST (CBool true)}
| integer as s { CONST (CInt (int_of_string s)) }
| string as s { CONST (CString s)}

| "==" { EQ }
| "<>" { NEQ }
| "<" { LT }
| "<=" { LEQ }
| ">" { GT }
| ">=" { GEQ }
| "+" { PLUS }
| "-" { MINUS }
| "*" { TIMES }
| "/" { SLASH }
| "and" { AND }
| "or" { OR }

| '(' { LEFTPAR }
| ')' { RIGHTPAR }

| "var" { VAR }
| "block:" { BLOCK } 
| "cases:" { CASES }
| "end" { END } 
| "for" { FOR }
| "from" { FROM }
| "fun" { FUN }
| "if" { IF }
| "else:" { ELSE }
| "lam" { LAM }
| eof { EOF }
| _ as c { failwith ("illegal character" ^ String.make 1 c) }