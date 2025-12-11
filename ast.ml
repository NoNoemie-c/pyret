
type binop =
| BEq | BNeq | BLt | BLeq | BGt | BGeq | BAdd | BSub | BMul | BDiv | BAnd | BOr
type var = string (* pour l'instant *)
type const =
| CBoolean of bool
| CNumber of int
| CString of string
type typ = 
| TVar of var * typ list
| TArrow of typ list * typ
type expr =
| EConst of const
| EBlock of block
| EOp of binop * expr list
| EIf of (expr * block) list * block
| EVar of var
| ECall of caller * expr list
| ECases of typ * expr * (var * var list * block) list
| ELam of funbody
and stmt = 
| SExpr of expr
| SDecl of bool * var * typ option * expr
| SAssign of var * expr
| SFun of var * var list * funbody
and funbody = (var * typ) list * typ * block
and block = stmt list
and caller = 
| CVar of var
| CCall of caller * expr list
type file = block

let str_of_binop = function
| BEq -> "==" | BNeq -> "<>" 
| BLt -> "<" | BLeq -> "<=" | BGt -> ">"| BGeq -> ">=" 
| BAdd -> "+" | BSub -> "-" | BMul -> "*" | BDiv -> "/" 
| BAnd -> "and" | BOr -> "or"
let rec pp_separated_list pp fmt = function
| [] -> ()
| [x] -> pp fmt x
| x::ll -> pp fmt x; 
  Format.fprintf fmt ", ";
  pp_separated_list pp fmt ll 
let rec pp_block fmt = 
  List.iter (Format.fprintf fmt "%a\n" pp_stmt)
and pp_var fmt = Format.fprintf fmt "%s"
and pp_stmt fmt = function
| SExpr e -> pp_expr fmt e
| SDecl(b, i, t, e) -> 
  let pp_typ_opt f = function
  | None -> ()   | Some tt -> Format.fprintf f " :: %a " pp_typ tt in
  Format.fprintf fmt "%s%a%a = %a" (if b then "var " else "") pp_var i pp_typ_opt t pp_expr e
| SAssign(i, e) -> Format.fprintf fmt "%s := %a" i pp_expr e
| SFun (i, l, f) -> Format.fprintf fmt "%a<%a>%a" 
  pp_var i (pp_separated_list pp_var) l pp_funbody f
and pp_const fmt = function
| CBoolean b -> Format.fprintf fmt (if b then "true" else "false")
| CNumber i -> Format.fprintf fmt "%d" i
| CString s -> Format.fprintf fmt "\"%s\"" s
and pp_expr fmt = function
| EConst c -> pp_const fmt c
| EOp (op, args) -> Format.fprintf fmt "[%s : %a]" (str_of_binop op)
  (pp_separated_list pp_expr) args
| EBlock b -> pp_block fmt b
| EIf (l, eb) ->
  let ic, ib = List.hd l in
  Format.fprintf fmt "if %a : %a" pp_expr ic pp_block ib; 
  List.iter (fun (eic, eib) -> 
    Format.fprintf fmt "else if %a: %a" pp_expr eic pp_block eib)
    (List.tl l);
  if eb <> [] then
    Format.fprintf fmt "else: %a" pp_block eb; 
  Format.fprintf fmt "end"
| EVar i -> Format.fprintf fmt "%a" pp_var i
| ECall (c, l) -> Format.fprintf fmt "%a(%a)" pp_caller c 
  (pp_separated_list pp_expr) l
| ECases (t, e, l) ->
  Format.fprintf fmt "cases (%a) %a:\n" pp_typ t pp_expr e;
  List.iter (fun (v, vl, b) -> Format.fprintf fmt "| %a(%a) => %a" 
    pp_var v (pp_separated_list pp_var) vl pp_block b)
    l
| ELam f -> Format.fprintf fmt "lam%a" pp_funbody f
and pp_funbody fmt (p, t, b) =
  Format.fprintf fmt "(%a) -> %a:\n%a" 
  (pp_separated_list pp_param) p pp_typ t pp_block b;
and pp_param fmt (i, t) = Format.fprintf fmt "%a::%a" pp_var i pp_typ t
and pp_caller fmt = function
| CVar i -> Format.fprintf fmt "%a" pp_var i
| CCall (c, l) -> Format.fprintf fmt "%a(%a)" pp_caller c 
  (pp_separated_list pp_expr) l
and pp_typ fmt = function
| TArrow (l, r) -> 
  Format.fprintf fmt "(%a -> %a)" (pp_separated_list pp_typ) l pp_typ r
| TVar (i, l) -> 
  Format.fprintf fmt "%a<%a>" pp_var i (pp_separated_list pp_typ) l

let print_file = Format.fprintf Format.std_formatter "%a\n" pp_block 