
%{
  open Ast
  open Lexing
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
{ s::f }
| EOF { [] }

voidstmt:
| option(VAR) i=IDENTEQUAL e=bexpr 
  { SDecl (i, None, e) }
| option(VAR) i=IDENTCOLONCOLON t=typ EQUAL e=bexpr 
  { SDecl (i, Some t, e) }
| option(VAR) i=IDENTCOLONCOLON t=IDENTEQUAL e=bexpr 
  { SDecl (i, Some (TVar (t, [])), e) }
| FUN i=IDENTLP f=funbody { SFun (i, [], f) }
| FUN i=IDENT either(LA, LT) l=separated_nonempty_list(COMMA, IDENT) RA
  LP f=funbody { SFun (i, l, f) }
stmt:
| i=IDENTCOLONEQUAL e=bexpr { SAssign (i, e) } 
| e=bexpr { SExpr e }

voidblock:
| s=stmt { [s] }
| s=voidstmt b=block 
{ s::b }
block:
| s=stmt { [s] }
| s=stmt b=block | s=voidstmt b=block
{ s::b }

bexpr:
| anyLP e=bexpr RP b=binop anyLP el=bexpr RP 
| e=expr b=binop anyLP el=bexpr RP { EOp (b, [e; el]) }
| anyLP e=bexpr RP b=binop el=bexpr 
| e=expr b=binop el=bexpr{ 
  match el with 
  | EOp (bl, l) when bl = b -> EOp (b, e::l) 
  | EOp _ -> exit 1
  | ef -> EOp (b, [e; ef])
}
| i=IDENTEQ el = bexpr {
  match el with 
  | EOp (BEq, l) -> EOp (BEq, EVar i::l) 
  | EOp _ -> exit 1
  | ef -> EOp (BEq, [EVar i; ef]) 
}
| anyLP e=bexpr RP { e } 
| e0=expr { e0 }

ublock:
| COLON b=voidblock { b }
| BLOCK b=block { b }

typ:
| LP l=separated_list(COMMA, typ) ARROW r=typ RP { TArrow (l, r) }
| i=IDENT l=loption(LA x=separated_nonempty_list(COMMA, typ) either(GT, RA) { x }) 
{ TVar (i, l) }

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
| c=CONST { EConst c }
| BLOCK b=block END { EBlock b }
| CASES t=typ RP e=bexpr BLOCK
  l=list(branch) END
  { ECases (t, e, l) }
| CASES t=typ RP e=bexpr COLON
  l=list(voidbranch) END
  { ECases (t, e, l) }
| c=caller { match c with CCall (k, p) -> ECall (k, p) | CVar i -> EVar i }
| IF ic=bexpr BLOCK ib=block 
  l=list(ELSEIF eic=bexpr COLON eib=block { (eic, eib) })
  ELSE eb=block END { EIf ((ic, ib)::l, eb) } 
| IF ic=bexpr COLON ib=voidblock 
  l=list(ELSEIF eic=bexpr COLON eib=voidblock { (eic, eib) })
  ELSE eb=voidblock END { EIf ((ic, ib)::l, eb) } 
| LAM f=funbody { ELam f }
| FOR c=callerLP l=separated_list(COMMA, from) RP ARROW t=typ b=ublock END
  { ECall (c, ELam (List.map fst l, t, b)::List.map snd l) }

branch:
| BAR i=IDENTDARROW b=block {i, [], b}
| BAR i=IDENTLP p=separated_nonempty_list(COMMA, IDENT) RP
    DARROW b=block {i, p, b}
voidbranch:
| BAR i=IDENTDARROW b=voidblock {i, [], b}
| BAR i=IDENTLP p=separated_nonempty_list(COMMA, IDENT) RP
    DARROW b=voidblock {i, p, b}

from: 
| p=param FROM e=bexpr { (p, e) }

caller:
| i=IDENT { CVar i }
| i=IDENTLP l=separated_nonempty_list(RPLP, separated_list(COMMA, bexpr)) RP
  { List.fold_left (fun c ll -> CCall (c, ll)) (CVar i) l }
callerLP:
| i=IDENTLP { CVar i }
| i=IDENTLP l=separated_nonempty_list(RPLP, separated_list(COMMA, bexpr)) RPLP
  { List.fold_left (fun c ll -> CCall (c, ll)) (CVar i) l }

binop:
| c=CMP { c }
| LT { BLt }
| GT { BGt } 