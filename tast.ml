type tvar = { id : int; mutable def : typ option }
and typ = 
| TTAny | TTNothing | TTBoolean | TTNumber | TTString | TTList of typ
| TTVar of tvar
| TTArrow of typ list * typ
let rec head = function
| TTVar { def = Some d; id = _ } -> head d
| t -> t
let rec canon = function
| TTList a -> TTList (canon a)
| TTArrow (a, b) -> TTArrow (List.map canon a, canon b)
| TTVar { def=Some d; id = _ } ->  canon d
| x -> x

type var = string
type typd = {e: expr; t: typ}
and expr =
| TEConst of Ast.const
| TEBlock of block
| TEOp of Ast.binop * typd list
| TEIf of (typd * block) list * block
| TEVar of var
| TECall of caller * typd list
| TECases of typd * (var * var list * block) list
| TELam of funbody
and stmt = 
| TSExpr of typd
| TSDecl of var * typd
| TSAssign of var * typd
| TSFun of var * var list * funbody
and funbody = param list * block
and param = var * typ
and block = stmt list
and caller = 
| TCVar of var
| TCCall of caller * typd list
type file = block

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
| TSExpr e -> pp_typd fmt e
| TSDecl(i, e) -> 
  Format.fprintf fmt "var %a = %a" pp_var i pp_typd e
| TSAssign(i, e) -> Format.fprintf fmt "%s := %a" i pp_typd e
| TSFun (i, l, f) -> Format.fprintf fmt "%a<%a>%a" 
  pp_var i (pp_separated_list pp_var) l pp_funbody f
and pp_expr fmt = function
| TEConst c -> Ast.pp_const fmt c
| TEOp (op, args) -> Format.fprintf fmt "[%s : %a]" (Ast.str_of_binop op)
  (pp_separated_list pp_typd) args
| TEBlock b -> pp_block fmt b
| TEIf (l, eb) ->
  let ic, ib = List.hd l in
  Format.fprintf fmt "if %a : %a" pp_typd ic pp_block ib; 
  List.iter (fun (eic, eib) -> 
    Format.fprintf fmt "else if %a: %a" pp_typd eic pp_block eib)
    (List.tl l);
  if eb <> [] then
    Format.fprintf fmt "else: %a" pp_block eb; 
  Format.fprintf fmt "end"
| TEVar i -> Format.fprintf fmt "%a" pp_var i
| TECall (c, l) -> Format.fprintf fmt "%a(%a)" pp_caller c 
  (pp_separated_list pp_typd) l
| TECases (e, l) ->
  Format.fprintf fmt "cases %a:\n" pp_typd e;
  List.iter (fun (v, vl, b) -> Format.fprintf fmt "| %a(%a) => %a" 
    pp_var v (pp_separated_list pp_var) vl pp_block b)
    l
| TELam f -> Format.fprintf fmt "lam%a" pp_funbody f
and pp_funbody fmt (p, b) =
  Format.fprintf fmt "(%a):\n%a" 
  (pp_separated_list pp_param) p pp_block b;
and pp_param fmt (i, t) = Format.fprintf fmt "%a::%a" pp_var i pp_typ t
and pp_caller fmt = function
| TCVar i -> Format.fprintf fmt "%a" pp_var i
| TCCall (c, l) -> Format.fprintf fmt "%a(%a)" pp_caller c 
  (pp_separated_list pp_typd) l
and pp_typ fmt t = match (head t) with
| TTAny -> Format.fprintf fmt "Any"
| TTNothing -> Format.fprintf fmt "Nothing"
| TTBoolean -> Format.fprintf fmt "Boolean"
| TTNumber -> Format.fprintf fmt "Number"
| TTString -> Format.fprintf fmt "String"
| TTList a -> Format.fprintf fmt "List<%a>" pp_typ a
| TTArrow (l, r) -> 
  Format.fprintf fmt "(%a -> %a)" (pp_separated_list pp_typ) l pp_typ r
| TTVar v -> Format.fprintf fmt "'%d" v.id
and pp_typd fmt t =
  Format.fprintf fmt "%a {%a}" pp_expr t.e pp_typ t.t

let print_file = Format.fprintf Format.std_formatter "%a\n" pp_block 