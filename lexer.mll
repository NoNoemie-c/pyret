{
  open Lexing
  open Ast
  open Parser

  exception Error

  let cdepth = ref 0
}

let linecomment = "#" [^ '|'] [^ '\n']* "\n" 
  (* | "#|" ([^ '|']* '|')+ "#"  *)
let whitespace = [' ' '\t' '\n']+
let digit     = ['0'-'9']
let integer = ("+" | "-")? digit+
let letter = ['a'-'z' 'A'-'Z' '_']
let escape = "\\\\" | "\\\'" | "\\\"" | "\\t" | "\\n"
let string = "\"" ([^'"' '\\'] | escape)* "\"" 
  | "\'" ([^'\'' '\\'] | escape)* "\'"
let ident = letter ("-"* (letter | digit)+)*

rule comment = parse
| "#|" { 
  incr cdepth; 
  token lexbuf 
}
| "|#" { 
  decr cdepth;
  if !cdepth = 0 then token lexbuf 
  else comment lexbuf }
| eof { Printf.eprintf "unending comment"; raise Error }
| _ { comment lexbuf }
and token = parse
| linecomment | whitespace { token lexbuf }
| "#|" { incr cdepth; comment lexbuf }
| "|#" { Printf.eprintf "unmatched |#"; raise Error }

| "false" { CONST (CBool false) }
| "true" { CONST (CBool true)}
| integer as s { CONST (CInt (int_of_string s)) }
| string as s { CONST (CString s)}

| " == " { EQ } | " <> " { NEQ }
| " < " { LT } | " <= " { LEQ } | " > " { GT } | " >= " { GEQ }
| " + " { PLUS } | " - " { MINUS } | " * " { TIMES } | " / " { SLASH }
| " and " { AND }  | " or " { OR }

| ":" { COLON } | " := " { COLONEQUAL } | "::" { COLONCOLON }
| " => " { DARROW }
| "<" { LA } | ">" { RA }
| "->" { ARROW }
| "| " { BAR }

| '(' { LP } | ')' { RP }

| "var" { VAR }
| "block" { BLOCK } 
| "cases" { CASES }
| "end" { END } 
| "for" { FOR }
| "from" { FROM }
| "fun" { FUN }
| "if" { IF }
| "else if" { ELSEIF }
| "else" { ELSE }
| "lam" { LAM }

| ident as i { IDENT i }

| eof { EOF }
| _ { 
  let p = lexbuf.lex_curr_p in
  Printf.eprintf "%d:%d : <$<$ %s $>$> cant be matched to a token\n"
    p.pos_lnum (p.pos_cnum - p.pos_bol) (Lexing.lexeme lexbuf);
  raise Error
 }