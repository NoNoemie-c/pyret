{
  open Lexing
  open Ast
  open Parser

  let cdepth = ref 0
  let keywords = Hashtbl.create 17
  let () = 
    List.iter (fun x -> Hashtbl.add keywords x ()) ["var" ; "block"; "cases"; 
    "end"; "for"; "from"; "fun"; "if"; "else if"; "else"; "lam"; "true"; 
    "false"]
  let notkw s =
    if Hashtbl.mem keywords s then raise (Error.Lexer (fun () -> 
      Printf.eprintf "unexpected keyword \"%s\"\n" s))
    else s
  let line lb =
    String.iter (fun c -> if c = '\n' then new_line lb) (Lexing.lexeme lb)
}

let ws = [' ' '\t' '\n' '\r']
let linecomment = "#\n" | "#" [^ '|'] [^ '\n']* 
let osef = (ws | linecomment)*
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
  if !cdepth = 0 then tokens lexbuf 
  else comment lexbuf }
| eof { raise (Error.Lexer (fun () -> Printf.eprintf "unending comment")) }
| _ { comment lexbuf }
and tokens = parse
| osef { line lexbuf; tokens lexbuf }
| "#" eof { [EOF] }
| "#|" { incr cdepth; comment lexbuf }
| "|#" { raise (Error.Lexer (fun () -> Printf.eprintf "unmatched |#")) }

| "false" { [CONST (CBoolean false)] }
| "true" { [CONST (CBoolean true)] }
| integer as s { [CONST (CNumber (int_of_string s))] }
| string as s { [CONST (CString (String.sub s 1 (String.length s - 2)))] }

| ws"=="ws { line lexbuf; [CMP BEq] } | ws"<>"ws { line lexbuf; [CMP BNeq] }
| ws"<"ws { line lexbuf; [LT] } | ws"<="ws { line lexbuf; [CMP BLeq] } 
| ws">"ws { line lexbuf; [GT] } | ws">="ws { line lexbuf; [CMP BGeq] }
| ws"+"ws { line lexbuf; [CMP BAdd] } | ws"-"ws { line lexbuf; [CMP BSub] } 
| ws"*"ws { line lexbuf; [CMP BMul] } | ws"/"ws { line lexbuf; [CMP BDiv] }
| ws"and"ws { line lexbuf; [CMP BAnd] }  | ws"or"ws { line lexbuf; [CMP BOr] }

| ":" { [COLON] } 
| "," { [COMMA] } | "=" { [EQUAL] }
| "=>"ws { line lexbuf; [DARROW] }
| "<" { [LA] } | ">" { [RA] }
| "->" { [ARROW] }
| "|" { [BAR] }

| ")(" { [RPLP] }
| " (" { [SLP] }

| "(" { [LP] } | ")" { [RP] }

| "var" { [VAR] }
| "block:" { [BLOCK] } 
| "cases" osef "(" { line lexbuf; [CASES] }
| "end" { [END] } 
| "for" { [FOR] }
| "from" { [FROM] }
| "fun" { [FUN] }
| "if" { [IF] }
| "else if" { [ELSEIF] }
| "else:" { [ELSE] }
| "lam(" { [LAM] }

| [^' ' '\t' '\n']['+' '-']integer { [raise (Error.Lexer (fun () ->
  Printf.eprintf "hardcoded error for \"espace\" tests\n"))] }

| ident as i osef ":: " { line lexbuf; [IDENTCOLONCOLON (notkw i)] }
| ident as i osef "=" { line lexbuf; [IDENTEQUAL (notkw i)] }
| ident as i osef "=>"ws { line lexbuf; [IDENTDARROW (notkw i)] }
| ident as i " == " { [IDENT (notkw i); CMP BEq] }
| ident as i osef ":=" { line lexbuf; [IDENTCOLONEQUAL (notkw i)] }
| ident as i "(" { [IDENTLP (notkw i)] }
| ident as i { [IDENT (notkw i)] }

| eof { [EOF] }
| _ {
  raise (Error.Lexer (fun () -> 
    Printf.eprintf "\"%s\" cant be matched to a token\n" 
      (Lexing.lexeme lexbuf)))
}

{
  let token =
    let q = Queue.create () in (* prochains lexèmes à renvoyer *)
    fun lb ->
      if Queue.is_empty q then begin
        let l = tokens lb in
        List.iter (fun t -> Queue.add t q) l
      end;
      Queue.pop q
}