
/* Analyseur syntaxique pour Mini-Python */

%{
  exception LexingError
  open Ast
%}

%token <Ast.const> CONST
%token <Ast.binop> CMP
%token <Ast.var> IDENT
%token EOF
%token LP RP LSQ RSQ COMMA EQUAL COLON
%token EQ NEQ LT LEQ GT GEQ PLUS MINUS TIMES SLASH AND OR
%token BLOCK CASES ELSE END FALSE FOR FROM FUN IF LAM TRUE VAR

/* Définitions des priorités et associativités des tokens */


/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.file> file

%%

file:
| l = list(stmt) EOF { l }
;

stmt:
| e = expr { SExpr e }
;

expr:
| c = CONST { EConst c }
| e0 = expr l = nonempty_list(b=binop e=expr { (b, e) }) 
  { let b = fst (List.hd l) in
    if List.exists (fun (a, _) -> a <> b) l then raise LexingError;
    EOp (b, e0::(List.map snd l)) }
;

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
;