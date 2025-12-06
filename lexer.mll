{
  open Lexing
  open Ast
  open Parser

  exception Error

  let cdepth = ref 0
}

let linecomment = "#\n" | "#" [^ '|'] [^ '\n']*
let whitespace = [' ' '\t' '\n']+
let osef = (whitespace | linecomment)*
let digit     = ['0'-'9']
let integer = ("+" | "-")? digit+
let letter = ['a'-'z' 'A'-'Z' '_']
let escape = "\\\\" | "\\\'" | "\\\"" | "\\t" | "\\n"
let string = "\"" ([^'"' '\\' '\n'] | escape)* "\"" 
  | "\'" ([^'\'' '\\' '\n'] | escape)* "\'"
let ident = letter ("-"* (letter | digit)+)*

rule comment = parse
| "#|" { 
  incr cdepth; 
  comment lexbuf 
}
| "|#" { 
  decr cdepth;
  if !cdepth = 0 then token lexbuf 
  else comment lexbuf }
| eof { Printf.eprintf "unending comment"; raise Error }
| _ { comment lexbuf }
and token = parse
| osef { token lexbuf }
| linecomment eof | "#" eof { EOF }
| "#|" { incr cdepth; comment lexbuf }
| "|#" { Printf.eprintf "unmatched |#"; raise Error }

| "false" { CONST (CBool false) }
| "true" { CONST (CBool true)}
| "empty" { CONST CEmpty }
| integer as s { CONST (CInt (int_of_string s)) }
| string as s { CONST (CString (String.sub s 1 (String.length s - 1)))}

| (integer | ident) "+" integer { Printf.eprintf "+ without spaces"; raise Error }
| (integer | ident) "-" integer { Printf.eprintf "+ without spaces"; raise Error }
| " == " { CMP BEq } | " <> " { CMP BNeq }
| " < " { LT } | " <= " { CMP BLeq } | " > " { GT } | " >= " { CMP BGeq }
| " + " { CMP BAdd } | " - " { CMP BSub } | " * " { CMP BMul } | " / " { CMP BDiv }
| " and " { CMP BAnd }  | " or " { CMP BOr }

| ":" { COLON } | ":=" { COLONEQUAL } | " :: " { COLONCOLON }
| "," { COMMA } | "=" { EQUAL }
| " => " { DARROW }
| "<" { LA } | ">" { RA }
| "->" { ARROW }
| "|" { BAR }

| ")(" { RPLP }
| "\n(" { CRLP }
| " (" { SLP }

| "(" { LP } | ")" { RP }

| "var" { VAR }
| "block:" { BLOCK } 
| "cases" { CASES }
| "end" { END } 
| "for" { FOR }
| "from" { FROM }
| "fun" { FUN }
| "if" { IF }
| "else if" { ELSEIF }
| "else:" { ELSE }
| "lam" { LAM }

| ident as i osef ":: " { IDENTCOLONCOLON i }
| ident as i osef "=" { IDENTEQUAL i }
| ident as i osef ":=" { IDENTCOLONEQUAL i }
| ident as i "(" { IDENTLP i }
| ident as i { IDENT i }

| eof { EOF }
| _ { 
  let p = lexbuf.lex_curr_p in
  Printf.eprintf "%d:%d : <$ %s $> cant be matched to a token\n"
    p.pos_lnum (p.pos_cnum - p.pos_bol) (Lexing.lexeme lexbuf);
  raise Error
 }