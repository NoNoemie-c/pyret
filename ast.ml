
type binop =
| BEq | BNeq | BLt | BLeq | BGt | BGeq | BAdd | BSub | BMul | BDiv | BAnd | BOr
let str_of_binop = function
| BEq -> "==" | BNeq -> "<>" | BLt -> "<" | BLeq -> "<=" | BGt -> ">"| BGeq -> ">=" | BAdd -> "+" | BSub -> "-" | BMul -> "*" | BDiv -> "/" | BAnd -> "and" | BOr -> "or"
type var = string (* pour l'instant *)
type const =
| CBool of bool
| CInt of int
| CString of string
type expr =
| EConst of const
| EOp of (binop * expr list)
and stmt = 
| SExpr of expr
type file = stmt list

let rec pp_file fmt = 
  List.iter (Format.fprintf fmt "%a\n" pp_stmt)
and pp_stmt fmt = function
| SExpr e -> pp_expr fmt e
and pp_const fmt = function
| CBool b -> Format.fprintf fmt (if b then "true" else "false")
| CInt i -> Format.fprintf fmt "%d" i
| CString s -> Format.fprintf fmt "\"%s\"" s
and pp_expr fmt = function
| EConst c -> pp_const fmt c
| EOp (op, args) -> Format.fprintf fmt "[%s : %a]" (str_of_binop op)
  (fun f -> List.iter (pp_expr f)) args

(* let rec pp_typ fmt = function
  | Tproduct (t1, t2) -> Format.fprintf fmt "%a *@ %a" pp_atom t1 pp_atom t2
  | Tarrow (t1, t2) -> Format.fprintf fmt "%a ->@ %a" pp_atom t1 pp_typ t2
  | (Tint | Tvar _) as t -> pp_atom fmt t
and pp_atom fmt = function
  | Tint -> Format.fprintf fmt "int"
  | Tvar v -> pp_tvar fmt v
  | Tarrow _ | Tproduct _ as t -> Format.fprintf fmt "@[<1>(%a)@]" pp_typ t
and pp_tvar fmt = function
  | { def = None; id } -> Format.fprintf fmt "'%d" id
  | { def = Some t; id } -> Format.fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t
 *)

let print_file = pp_file Format.std_formatter