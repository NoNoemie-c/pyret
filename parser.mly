
%{
  open Ast
  open Lexing

  let sep_stmt ep1 sp2 =
    if ep1.pos_lnum = sp2.pos_lnum then
      raise (Error.Parser (ep1, sp2, fun () ->
        Printf.eprintf "can't have two statements on the same line\n"))
%}

%token <Ast.const> CONST
%token <Ast.binop> CMP
%token <Ast.var> IDENT IDENTLP IDENTCOLONEQUAL IDENTEQUAL 
  IDENTCOLONCOLON IDENTDARROW IDENTEQ
%token EOF
%token SLP LP RP RPLP LA RA LT GT
%token COMMA EQUAL COLON ARROW DARROW BAR
%token BLOCK CASES IF ELSEIF ELSE END FOR FROM FUN LAM VAR

%start file
%type <Ast.file> file
%%

file:
| s=stmt f=file | s=voidstmt f=file
{ sep_stmt $endpos(s) $startpos(f); s::f }
| EOF { [] }

voidstmt:
| b=boption(VAR) i=IDENTEQUAL e=bexpr 
  { { x=SDecl (b, i, None, e); sp=$startpos; ep=$endpos } }
| b=boption(VAR) i=IDENTCOLONCOLON t=typ EQUAL e=bexpr 
  { { x=SDecl (b, i, Some t, e); sp=$startpos; ep=$endpos } }
| b=boption(VAR) i=IDENTCOLONCOLON t=IDENTEQUAL e=bexpr 
  { { x=SDecl (b, i, Some ({x=TVar (t, []); sp=$startpos(b); ep=$endpos(b) }), e); sp=$startpos; ep=$endpos } }
| FUN i=IDENTLP f=funbody { { x=SFun (i, [], f); sp=$startpos; ep=$endpos } }
| FUN i=IDENT either(LA, LT) l=separated_nonempty_list(COMMA, IDENT) RA
  LP f=funbody { { x=SFun (i, l, f); sp=$startpos; ep=$endpos } }
stmt:
| i=IDENTCOLONEQUAL e=bexpr { { x=SAssign (i, e); sp=$startpos; ep=$endpos } } 
| e=bexpr { { x=SExpr e; sp=$startpos; ep=$endpos } }

voidblock:
| s=stmt { [s] }
| s=voidstmt b=block 
{ sep_stmt $endpos(s) $startpos(b); s::b }
block:
| s=stmt { [s] }
| s=stmt b=block | s=voidstmt b=block
{ sep_stmt $endpos(s) $startpos(b); s::b }

bexpr:
| anyLP e=bexpr RP b=binop anyLP el=bexpr RP 
| e=expr b=binop anyLP el=bexpr RP { { x=EOp (b, [e; el]); sp=$startpos; ep=$endpos } }
| anyLP e=bexpr RP b=binop el=bexpr 
| e=expr b=binop el=bexpr{ {
  x=(match el.x with 
  | EOp (bl, l) when bl = b -> EOp (b, e::l) 
  | EOp _ -> raise (Error.Parser ($startpos(el), $endpos(el), fun () ->
    Printf.eprintf "operator doesnt match the one found on its left"))
  | _ -> EOp (b, [e; el]));
  sp=$startpos; ep=$endpos 
} }
| i=IDENTEQ el = bexpr { {
  x=(match el.x with 
  | EOp (BEq, l) -> EOp (BEq, { x=EVar i; sp=$startpos(i); ep=$endpos(i) }::l) 
  | EOp _ -> raise (Error.Parser ($startpos(el), $endpos(el), fun () ->
    Printf.eprintf "operator doesnt match the one found on its left"))
  | _ -> EOp (BEq, [{ x=EVar i; sp=$startpos(i); ep=$endpos(i) }; el]));
  sp=$startpos; ep=$endpos 
} }
| anyLP e=bexpr RP { e } 
| e0=expr { e0 }

ublock:
| COLON b=voidblock { b }
| BLOCK b=block { b }

typ:
| LP l=separated_list(COMMA, typ) ARROW r=typ RP { { x=TArrow (l, r); sp=$startpos; ep=$endpos } }
| i=IDENT l=loption(LA x=separated_nonempty_list(COMMA, typ) either(GT, RA) { x }) 
{ { x=TVar (i, l); sp=$startpos; ep=$endpos } }

param:
| i=IDENTCOLONCOLON t=typ { (i, t) }

funbody:
| l=separated_list(COMMA, param) RP ARROW r=typ BLOCK b = block END { (l, r, b) }
| l=separated_list(COMMA, param) RP ARROW r=typ COLON b = voidblock END { (l, r, b) }

either (a, b):
| a { } | b { }
anyLP:
| LP | SLP { }
expr:
| c=CONST { { x=EConst c; sp=$startpos; ep=$endpos } }
| BLOCK b=block END { { x=EBlock b; sp=$startpos; ep=$endpos } }
| CASES t=typ RP e=bexpr BLOCK
  l=list(branch) END
  { { x=ECases (t, e, l); sp=$startpos; ep=$endpos } }
| CASES t=typ RP e=bexpr COLON
  l=list(voidbranch) END
  { { x=ECases (t, e, l); sp=$startpos; ep=$endpos } }
| c=caller { { x=(match c.x with CCall (k, p) -> ECall (k, p) | CVar i -> EVar i); sp=$startpos; ep=$endpos } }
| IF ic=bexpr BLOCK ib=block 
  l=list(ELSEIF eic=bexpr COLON eib=block { (eic, eib) })
  ELSE eb=block END { { x=EIf ((ic, ib)::l, eb); sp=$startpos; ep=$endpos } }
| IF ic=bexpr COLON ib=voidblock 
  l=list(ELSEIF eic=bexpr COLON eib=voidblock { (eic, eib) })
  ELSE eb=voidblock END { { x=EIf ((ic, ib)::l, eb); sp=$startpos; ep=$endpos } } 
| LAM f=funbody { { x=ELam f; sp=$startpos; ep=$endpos } }
| FOR c=callerLP l=separated_list(COMMA, from) RP ARROW t=typ b=ublock END
  { { x=ECall (c, 
    { x=ELam (List.map fst l, t, b); sp=$startpos(c); ep=$endpos(c) }::List.map snd l); sp=$startpos; ep=$endpos } }

branch:
| BAR i=IDENTDARROW b=block { i, [], b }
| BAR i=IDENTLP p=separated_nonempty_list(COMMA, IDENT) RP
    DARROW b=block { i, p, b }
voidbranch:
| BAR i=IDENTDARROW b=voidblock { i, [], b }
| BAR i=IDENTLP p=separated_nonempty_list(COMMA, IDENT) RP
    DARROW b=voidblock { i, p, b }

from: 
| p=param FROM e=bexpr { (p, e) }

caller:
| i=IDENT { { x=CVar i; sp=$startpos(i); ep=$endpos(i) } }
| i=IDENTLP l=separated_nonempty_list(RPLP, separated_list(COMMA, bexpr)) RP
  { { x=List.fold_left (fun c ll -> CCall ({ x=c; sp=$startpos(l); ep=$endpos(l)}, ll)) (CVar i) l; sp=$startpos; ep=$endpos } }
callerLP:
| i=IDENTLP { { x=CVar i; sp=$startpos(i); ep=$endpos(i) } }
| i=IDENTLP l=separated_nonempty_list(RPLP, separated_list(COMMA, bexpr)) RPLP
  { { x=List.fold_left (fun c ll -> CCall ({ x=c; sp=$startpos(l); ep=$endpos(l)}, ll)) (CVar i) l; sp=$startpos; ep=$endpos } }

binop:
| c=CMP { c }
| LT { BLt }
| GT { BGt } 