
/* Analyseur syntaxique pour Mini-Python */

%{
  open Ast
%}

%token <Ast.const> CONST
%token <Ast.binop> CMP
%token <Ast.var> IDENT
%token EOF
%token LP RP LA RA LSQ RSQ COMMA EQUAL COLON COLONCOLON COLONEQUAL ARROW DARROW
%token EQ NEQ LT LEQ GT GEQ PLUS MINUS TIMES SLASH AND OR BAR
%token BLOCK CASES IF ELSEIF ELSE END FOR FROM FUN LAM VAR

%start file
%type <Ast.file> file
%%

file:
| l=list(stmt) EOF { l }

stmt:
| e=bexpr { SExpr e }
| VAR i=IDENT t=option(COLONCOLON x=typ { x }) EQUAL e=bexpr 
  { SDecl (i, t, e) }
| i=IDENT COLONEQUAL e=bexpr { SAssign (i, e) } 

block:
| l=nonempty_list(stmt) { l }

bexpr:
| e0=expr l=nonempty_list(b=binop e=expr { (b, e) }) 
  { let b = fst (List.hd l) in
    if List.exists (fun (a, _) -> a <> b) l then exit 1;
    EOp (b, e0::(List.map snd l)) }
| e0=expr { e0 }

ublock:
| option(BLOCK) COLON b=block { b }

typ:
| LP l=list(typ) DARROW r=typ { TArrow (l, r) }
| i=IDENT l=option(LA x=separated_nonempty_list(COMMA, typ) RA { x }) 
{TVar (i, match l with None -> [] | Some x -> x) }

expr:
| i=IDENT { EVar i  }
| c=CONST { EConst c }
| LP e=bexpr RP { e }
| BLOCK COLON b=block END { EBlock b }
| CASES LP t=typ RP e=bexpr option(BLOCK) COLON
  l=list(BAR i=IDENT p=option(LP x=separated_list(COMMA, IDENT) RP { x }) 
    DARROW b=block {i, (match p with None -> [] | Some x -> x), b}) END
  { ECases (t, e, l) }
| c=call { c }
| IF ic=bexpr ib=ublock 
  l=list(ELSEIF eic=bexpr COLON eib=block { (eic, eib) })
  ELSE COLON eb=block END { EIf ((ic, ib)::l, eb) } 

call:
| i=IDENT l=nonempty_list(LP l=separated_list(COMMA, bexpr) RP { l }) 
  { match List.fold_left (fun c ll -> CCall (c, ll)) (CVar i) l with
    | CCall (k, p) -> ECall (k, p)
    | _ -> assert false 
  }

(*%inplace*) binop:
| EQ { BEq }
| NEQ { BNeq }
| LT { BLt }
| LEQ { BLeq }
| GT { BGt }
| GEQ { BGeq }
| PLUS  { BAdd }
| MINUS { BSub }
| TIMES { BMul }
| SLASH   { BDiv }
| AND   { BAnd }
| OR    { BOr  }