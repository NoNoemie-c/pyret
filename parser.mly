
/* Analyseur syntaxique pour Mini-Python */

%{
  open Ast
%}

%token <Ast.const> CONST
%token <Ast.binop> CMP
%token <Ast.var> IDENT
%token EOF
%token SLP LP RP LA RA
%token COMMA EQUAL COLON COLONCOLON COLONEQUAL ARROW DARROW
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
| FUN i=IDENT p=loption(LA l=separated_nonempty_list(COMMA, IDENT) RA { l })
  f=funbody { SFun (i, p, f) }

block:
| l=list(stmt) { l }

bexpr:
| e0=expr l=nonempty_list(b=binop e=expr { (b, e) }) 
  { let b = fst (List.hd l) in
    if List.exists (fun (a, _) -> a <> b) l then exit 1;
    EOp (b, e0::(List.map snd l)) }
| e0=expr { e0 }

ublock:
| COLON b=block { b }
| BLOCK COLON b=block e=expr { b @ [SExpr e] }
| BLOCK COLON b=block i=IDENT COLONEQUAL e=bexpr { b @ [SAssign (i, e)] }

typ:
| LP l=list(typ) DARROW r=typ { TArrow (l, r) }
| i=IDENT l=loption(LA x=separated_nonempty_list(COMMA, typ) RA { x }) 
{TVar (i, l) }

param:
| i=IDENT COLONCOLON t=typ { (i, t) }

funbody:
| LP l=separated_list(COMMA, param) RP DARROW r=typ b=ublock { (l, r, b) }

either (a, b):
| a { } | b { }
expr:
| i=IDENT { EVar i  }
| c=CONST { EConst c }
| LP e=bexpr RP { e }
| BLOCK COLON b=block END { EBlock b }
| CASES either(LP, SLP) t=typ RP e=bexpr option(BLOCK) COLON
  l=list(BAR i=IDENT p=loption(LP x=separated_list(COMMA, IDENT) RP { x }) 
    DARROW b=block {i, p, b}) END
  { ECases (t, e, l) }
| c=call { c }
| IF ic=bexpr ib=ublock 
  l=list(ELSEIF eic=bexpr COLON eib=block { (eic, eib) })
  ELSE COLON eb=block END { EIf ((ic, ib)::l, eb) } 
| LAM f=funbody { ELam f }
| FOR c=caller LP l=separated_list(COMMA, from) RP ARROW t=typ b=ublock END
  { ECall (c, ELam (List.map fst l, t, b)::List.map snd l) }

from: 
| p = param FROM e = expr { (p, e) }

caller: 
| i=IDENT l=nonempty_list(LP l=separated_list(COMMA, bexpr) RP { l }) 
  { List.fold_left (fun c ll -> CCall (c, ll)) (CVar i) l }

call:
| c=caller { match c with CCall (k, p) -> ECall (k, p) | _ -> assert false }

%inline binop:
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