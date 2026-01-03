open Ast
open Tast

let _dp = Lexing.dummy_pos

module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end
module Smap = Map.Make(String)
exception UnificationFailure of typ * typ
let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))
module Vset = Set.Make(V)
module Vmap = Map.Make(V)
  
type schema = { vars: Vset.t; typ: typ }
type env = { 
  bindings: (schema * bool) Smap.t; 
  fvars: Vset.t;
  tvars: tvar Smap.t
}

let rec ttyp_of_typ env t = match t.x with 
| TVar ("Any", []) -> TTAny
| TVar ("Nothing", []) -> TTNothing
| TVar ("Number", []) -> TTNumber
| TVar ("String", []) -> TTString
| TVar ("Boolean", []) -> TTBoolean
| TVar ("List", [a]) -> TTList (ttyp_of_typ env a)
| TArrow (l, r) -> TTArrow (List.map (ttyp_of_typ env) l, ttyp_of_typ env r)
| TVar (v, []) -> begin try TTVar (Smap.find v env.tvars)
  with Not_found -> raise (Error.Typer (_dp, _dp, fun () -> 
    Printf.eprintf "unknown type variable %s\n" v))
  end
| TVar (v, _) -> 
  raise (Error.Typer (_dp, _dp, fun () -> 
    Printf.eprintf "incorrect type arguments for %s\n" v))
  
let rec occur v t = 
  assert (v.def = None);
  match t with
| TTArrow (a, b) -> List.exists (occur v) a || occur v b
| TTVar wexpr -> if wexpr.def = None then v.id = wexpr.id 
  else occur v (head (TTVar wexpr)) 
| _ -> false

let rec unify env force t u = match head t, head u with
| TTAny, _ | _, TTAny -> ()
| TTArrow (at, bt), TTArrow (au, bu) -> 
  List.iter2 (unify env force) at au; 
  unify env force bt bu
| TTVar x, TTVar y when x = y -> ()
| TTVar v, z | z, TTVar v -> if occur v z 
  then unification_error (TTVar v) z
  else if Smap.exists (fun _ x -> x = v) env.tvars then 
  raise (Error.Typer (_dp, _dp, fun () -> 
    Printf.eprintf "generalized variable cannot be unified\n"))
  else if force then v.def <- Some z
| TTNumber, TTNumber | TTString, TTString | TTBoolean, TTBoolean
| TTNothing, TTNothing -> ()
| TTList a, TTList b -> unify env force a b
| _ -> unification_error t u

let rec unify_lt env force t u = match head t, head u with
| _, TTAny -> ()
| TTArrow (at, bt), TTArrow (au, bu) -> 
  List.iter2 (unify_lt env force) au at; unify_lt env force bt bu
| TTVar x, TTVar y when x = y -> ()
| z, TTVar v -> if occur v z 
  then unification_error (TTVar v) z
  else if force then v.def <- Some z
| TTVar v, z -> if occur v z 
  then unification_error (TTVar v) z
  else if Smap.exists (fun _ x -> x = v) env.tvars then 
    raise (Error.Typer (_dp, _dp, fun () -> 
      Printf.eprintf "generalized variable cannot be unified\n"))
  else if force then 
    ( v.def <- Some z)
| TTNumber, TTNumber | TTString, TTString | TTBoolean, TTBoolean
| TTNothing, TTNothing -> ()
| TTList a, TTList b -> unify_lt env force a b
| _ -> unification_error t u

let rec fvars t = match head t with
| TTArrow (a, b) -> List.fold_left Vset.union (fvars b) (List.map fvars a)
| TTVar wexpr -> Vset.singleton wexpr
| TTList a -> fvars a
| _ -> Vset.empty

let default = 
  let a = { id=0; def=None } in
  let b = { id=1; def=None } in
  let l = [
    "nothing", { vars=Vset.empty; typ=TTNothing };
    "num-modulo", { vars=Vset.empty; 
      typ=TTArrow ([TTNumber; TTNumber], TTNumber) };
    "empty", { vars=Vset.singleton a; typ=TTList (TTVar a) };
    "link", { vars=Vset.singleton a; 
      typ=TTArrow ([TTVar a; TTList (TTVar a)], TTList (TTVar a))};
    "print", { vars=Vset.singleton a; typ=TTArrow ([TTVar a], TTVar a)};
    "raise", { vars=Vset.singleton a; typ=TTArrow ([TTString], TTVar a)};
    "each", { vars=Vset.of_list [a; b]; typ=
      TTArrow ([TTArrow ([TTVar a], TTVar b); TTList (TTVar a)], TTNothing)};
    "fold", { vars = Vset.of_list [a; b]; typ=
      TTArrow ([TTArrow ([TTVar a; TTVar b], TTVar a); 
        TTVar a; TTList (TTVar b)], TTVar a)}
  ] in
  { bindings=List.fold_left (fun m (k, v) -> Smap.add k (v, false) m) 
    Smap.empty l; fvars=Vset.empty; tvars=Smap.empty }

let norm_varset s =
  Vset.fold (fun v s -> Vset.union (fvars (TTVar v)) s) s Vset.empty

let add gen x t b env =
  if Smap.mem x env.bindings then 
    raise (Error.Typer (_dp, _dp, fun () -> 
      Printf.eprintf "var %s was already declared\n" x));
  let vt = fvars t in
  let s, fvars =
    if gen then
      let env_fvars = norm_varset env.fvars in
      { vars = Vset.diff vt env_fvars; typ = t }, env.fvars
    else
      { vars = Vset.empty; typ = t }, Vset.union env.fvars vt
  in
  { bindings=Smap.add x (s, b) env.bindings; fvars=fvars; tvars=env.tvars }

let find s env =
  let scheme = fst (Smap.find s env.bindings) in
  let m = Vset.fold (fun v vm -> Vmap.add v (V.create ()) vm) 
    scheme.vars Vmap.empty in
  let rec refresh = function
  | TTArrow (a, b) -> TTArrow (List.map refresh a, refresh b)
  | TTList a -> TTList (refresh a)
  | TTVar v -> TTVar (try Vmap.find v m with Not_found -> v)
  | x -> x in
  refresh scheme.typ

let rec wexpr env e = match e.x with
| EConst (CBoolean _ as b) -> { e=TEConst b; t=TTBoolean }
| EConst (CNumber _ as n) -> { e=TEConst n; t=TTNumber }
| EConst (CString _ as s) -> { e=TEConst s; t=TTString }
| EOp (b, l) -> 
  begin 
    let ll = List.map (wexpr env) l in
    let tt = List.map (fun x -> x.t) ll in
    let t = match b with
    | BEq | BNeq -> TTBoolean
    | BLt | BLeq | BGt | BGeq -> 
      List.iter (fun t -> unify_lt env true t TTNumber) tt; TTBoolean
    | BAdd -> 
      (try 
        List.iter (fun t -> unify_lt env false t TTNumber) tt; 
        List.iter (fun t -> unify_lt env true t TTNumber) tt;
        TTNumber with
      | UnificationFailure _ -> 
        List.iter (fun t -> unify_lt env false t TTString) tt; 
        List.iter (fun t -> unify_lt env true t TTString) tt;
        TTString)
    | BSub | BMul | BDiv -> 
      List.iter (fun t -> unify_lt env true t TTNumber) tt; TTNumber
    | BAnd | BOr -> 
      List.iter (fun t -> unify_lt env true t TTBoolean) tt; TTBoolean in
    { e=TEOp (b, ll); t=t }
  end
| EVar v -> begin try { e=TEVar v; t=find v env }
  with Not_found -> raise (Error.Typer (e.sp, e.ep, fun () -> 
    Printf.eprintf "unbound variable %s\n" v)) end
| EBlock b -> let bb, t = wblock env b in 
  { e=TEBlock bb; t=t }
| EIf (l, eb) -> 
  let ll = List.map (fun (c, cb) -> (wexpr env c, wblock env cb)) l in
  let eeb, t = wblock env eb in
  { e=TEIf (List.map (fun (cc, (ccb, cbt)) -> 
    unify_lt env true cc.t TTBoolean; 
    unify env true cbt t; (cc, ccb)) ll, eeb); t=t }
| ECall ({ x=CVar f; sp=_; ep=_ }, l) -> 
  begin match (try find f env with Not_found -> 
    raise (Error.Typer (e.sp, e.ep, fun () -> 
      Printf.eprintf "unbound variable %s\n" f))) with
  | TTArrow (lt, rt) -> let ll = List.map (wexpr env) l in
    begin try 
      List.iter2 (unify_lt env true) (List.map (fun x -> x.t) ll) lt;
      { e=TECall (TCVar f, ll); t=rt } 
    with Invalid_argument _ -> raise (Error.Typer (e.sp, e.ep, fun () -> 
      Printf.eprintf "incorrect number of arguments for %s\n" f))
    end
  | _ -> raise (Error.Typer (e.sp, e.ep, fun () -> 
    Printf.eprintf "%s should have an arrow type\n" f)) 
  end
| ECall ({ x=CCall (c, p); sp=sp; ep=ep }, l) -> 
  let ec = wexpr env ({ x=ECall (c, p); sp=sp; ep=ep }) in
  begin match ec with
  | { e=TECall (cc, pp); t=TTArrow (lt, rt) } -> 
    let ll = List.map (wexpr env) l in
    begin try 
      List.iter2 (unify_lt env true) (List.map (fun x -> x.t) ll) lt;
      { e=TECall (TCCall (cc, pp), ll); t=rt } 
    with Invalid_argument _ -> raise (Error.Typer (e.sp, e.ep, fun () -> 
      Printf.eprintf "incorrect number of arguments for call\n"))
    end
  | _ -> raise (Error.Typer (e.sp, e.ep, fun () -> 
    Printf.eprintf "expr should have an arrow type\n")) 
  end
| ECases ({ x=TVar ("List", [a]); sp=_; ep=_ }, 
  c, ["empty", [], eb; "link", [x; y], lb])
| ECases ({ x=TVar ("List", [a]); sp=_; ep=_ }, 
  c, ["link", [x; y], lb; "empty", [], eb]) -> 
  let aa = ttyp_of_typ env a in
  let cc = wexpr env c in
  unify_lt env true cc.t (TTList aa);
  let env' = (if x = "_" then (fun u -> u) else add false x aa false)
    ((if y = "_" then (fun u -> u) else add false y (TTList aa) false) env) in
  let (eeb, et), (llb, lt) = wblock env' eb, wblock env' lb in 
  unify env true et lt;
  { e=TECases (cc, ["empty", [], eeb; "link", [x; y], llb]); t=lt }
| ELam (pl, t, b) ->
  let tt = ttyp_of_typ env t in
  let ppl = List.map (fun (p, pt) -> (p, ttyp_of_typ env pt)) pl in
  let env' = List.fold_left 
    (fun env (p, pt) -> add false p pt false env) env ppl in
  let bb, bt = wblock env' b in
  unify_lt env true bt tt;
  { e=TELam (ppl, bb); t=TTArrow (List.map snd ppl, tt) }
| _ -> raise (Error.Typer (e.sp, e.ep, fun () -> 
  Printf.eprintf "unrecognised expression\n"))
and wblock env = function
| [] -> assert false
| [s] -> let ss, _ = wstmt env s in
  [ss], (match ss with TSExpr ex | TSAssign (_, ex) -> ex.t | _ -> TTNothing)
| s::bb -> let ss, env' = wstmt env s in
  let l, t = wblock env' bb in
  ss::l, t
and wstmt env s = match s.x with 
| SExpr ex -> TSExpr (wexpr env ex), env
| SDecl (b, x, t, ex) -> 
  let s = wexpr env ex in
  begin match t with 
  | None -> ()
  | Some a -> unify_lt env true s.t (ttyp_of_typ env a)
  end;
  TSDecl (x, s), add (not b) x s.t b env
| SAssign (x, ex) ->
  begin try let ss = wexpr env ex in 
    if snd (Smap.find x env.bindings) then begin
      unify_lt env true ss.t (find x env); TSAssign (x, ss), env
    end else begin raise (Error.Typer (s.sp, s.ep, fun () -> 
      Printf.eprintf "variable %s is read-only\n" x))
    end
  with Not_found -> 
    raise (Error.Typer (s.sp, s.ep, fun () -> 
      Printf.eprintf "unbound variable %s\n" x)) end
| SFun (f, tl, (pl, t, b)) ->
  let vars = List.fold_left (fun m v-> 
    if List.mem v ["Any"; "Nothing"; "Number"; "String"; "Boolean"; "List"] then
      (raise (Error.Typer (s.sp, s.ep, fun () -> 
        Printf.eprintf "name already taken %s\n" v)))
    else Smap.add v (V.create ()) m) env.tvars tl in
  let env' = { env with tvars = vars } in
  let tt = ttyp_of_typ env' t in
  let ppl = List.map (fun (p, pt) -> (p, ttyp_of_typ env' pt)) pl in
  let env'' = add true f (TTArrow (List.map snd ppl, tt)) false env' in
  let env''' = List.fold_left 
    (fun aenv (p, pt) -> add false p pt false aenv) env'' ppl in
  let bb, bt = wblock env''' b in
  unify_lt env''' true bt tt;
  TSFun (f, tl, (ppl, bb)), env''
let w f = 
  try let b, _ = wblock default f in b
  with 
  | Not_found -> 
    raise (Error.Typer (_dp, _dp, fun () -> 
      Printf.eprintf "not found at the end of w\n"))
  | UnificationFailure _ -> 
    raise (Error.Typer (_dp, _dp, fun () -> 
      Printf.eprintf "unif at the end of w\n"))