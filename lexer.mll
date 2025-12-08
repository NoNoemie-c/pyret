{
  open Lexing
  open Ast
  open Parser

  exception Error

  let cdepth = ref 0
  let keywords = Hashtbl.create 17
  let () = 
    List.iter (fun x -> Hashtbl.add keywords x ()) ["var" ; "block"; "cases"; 
    "end"; "for"; "from"; "fun"; "if"; "else if"; "else"; "lam"; "true"; 
    "false"]
  let notkw s =
    if Hashtbl.mem keywords s then begin
      Printf.eprintf "unexpected keyword < %s >\n" s;
      raise Error
    end else s
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
| integer as s { CONST (CInt (int_of_string s)) }
| string as s { CONST (CString (String.sub s 1 (String.length s - 1)))}

| (integer | ident) "+" integer { Printf.eprintf "+ without spaces"; raise Error }
| (integer | ident) "-" integer { Printf.eprintf "_ without spaces"; raise Error }
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
| "lam(" { LAM }

| ident as i osef ":: " { IDENTCOLONCOLON (notkw i) }
| ident as i osef "=" { IDENTEQUAL (notkw i) }
| ident as i osef "=>" { IDENTDARROW (notkw i) }
| ident as i osef ":=" { IDENTCOLONEQUAL (notkw i) }
| ident as i "(" { IDENTLP (notkw i) }
| ident as i { IDENT (notkw i) }

| eof { EOF }
| _ { 
  let p = lexbuf.lex_curr_p in
  Printf.eprintf "%d:%d : <$ %s $> cant be matched to a token\n"
    p.pos_lnum (p.pos_cnum - p.pos_bol) (Lexing.lexeme lexbuf);
  raise Error
 }