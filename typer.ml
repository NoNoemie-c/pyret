open Ast
open Tast

exception Error

module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

let rec ttyp_of_typ = function
| TVar ("Any", []) -> TTAny
| TVar ("Nothing", []) -> TTNothing
| TVar ("Number", []) -> TTNumber
| TVar ("String", []) -> TTString
| TVar ("Boolean", []) -> TTBoolean
| TVar ("List", [a]) -> TTList (ttyp_of_typ a)
| TArrow (l, r) -> TTArrow (List.map ttyp_of_typ l, ttyp_of_typ r)
| TVar (v, _) -> 
  Printf.eprintf "unknown type or incorrect type arguments for %s" v; raise Error
  
exception UnificationFailure of typ * typ
let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))
  
let rec occur v t = 
  assert (v.def = None);
  match t with
| TTArrow (a, b) -> List.exists (occur v) a || occur v b
| TTVar w -> if w.def = None then v.id = w.id 
  else occur v (head (TTVar w)) 
| _ -> false

let rec unify t u = match head t, head u with
| TTArrow (at, bt), TTArrow (au, bu) -> List.iter2 unify at au; unify bt bu
| TTVar v, z | z, TTVar v -> if occur v z 
  then raise (unification_error (TTVar v) z)
  else v.def <- Some z
| TTNumber, TTNumber | TTString, TTString | TTBoolean, TTBoolean
| TTNothing, TTNothing | TTAny,  TTAny -> ()
| TTList a, TTList b -> unify a b
| _ -> unification_error t u

let rec unify_lt t u = match head t, head u with
| TTArrow (at, bt), TTArrow (au, bu) -> List.iter2 unify_lt au at; unify_lt bt bu
| TTVar v, z | z, TTVar v -> if occur v z 
  then raise (unification_error (TTVar v) z)
  else v.def <- Some z
| TTNumber, TTNumber | TTString, TTString | TTBoolean, TTBoolean
| TTNothing, TTNothing | _, TTAny -> ()
| TTList a, TTList b -> unify_lt a b
| _ -> unification_error t u

module Vset = Set.Make(V)

let rec fvars t = match head t with
| TTArrow (a, b) -> List.fold_left Vset.union (fvars b) (List.map fvars a)
| TTVar w -> Vset.singleton w
| TTList a -> fvars a
| _ -> Vset.empty
  
type schema = { vars : Vset.t; typ : typ }
module Smap = Map.Make(String)
type env = { bindings : (schema * bool) Smap.t; fvars : Vset.t }

let default = 
  let a = { id=0; def=None } in
  let b = { id=1; def=None } in
  let l = [
    "nothing", { vars=Vset.empty; typ=TTNothing };
    "num-modulo", { vars=Vset.empty; typ=TTArrow ([TTNumber; TTNumber], TTNumber) };
    "empty", { vars=Vset.singleton a; typ=TTList (TTVar a) };
    "link", { vars=Vset.singleton a; typ=TTArrow ([TTVar a; TTList (TTVar a)], TTList (TTVar a))};
    "print", { vars=Vset.singleton a; typ=TTArrow ([TTVar a], TTVar a)};
    "raise", { vars=Vset.singleton a; typ=TTArrow ([TTString], TTVar a)};
    "each", { vars=Vset.of_list [a; b]; typ=
      TTArrow ([TTArrow ([TTVar a], TTVar b); TTList (TTVar a)], TTNothing)};
    "fold", { vars = Vset.of_list [a; b]; typ=
      TTArrow ([TTArrow ([TTVar a; TTVar b], TTVar a); TTVar a; TTList (TTVar b)], TTVar a)}
  ] in
  { bindings=List.fold_left (fun m (k, v) -> Smap.add k (v, false) m) Smap.empty l; fvars=Vset.empty}

let norm_varset s =
  Vset.fold (fun v s -> Vset.union (fvars (TTVar v)) s) s Vset.empty

let add gen x t b e =
  if Smap.mem x e.bindings then 
    (Printf.eprintf "var %s was already declared\n" x; raise Error);
  let vt = fvars t in
  let s, fvars =
    if gen then
      let env_fvars = norm_varset e.fvars in
      { vars = Vset.diff vt env_fvars; typ = t }, e.fvars
    else
      { vars = Vset.empty; typ = t }, Vset.union e.fvars vt
  in
  { bindings = Smap.add x (s, b) e.bindings; fvars = fvars }

module Vmap = Map.Make(V)

let find s e =
  let scheme = try fst (Smap.find s e.bindings) with Not_found -> failwith "hm" in
  let m = Vset.fold (fun v vm -> Vmap.add v (V.create ()) vm) scheme.vars Vmap.empty in
  let rec refresh = function
  | TTArrow (a, b) -> TTArrow (List.map refresh a, refresh b)
  | TTList a -> TTList (refresh a)
  | TTVar v -> TTVar (try Vmap.find v m with Not_found -> v)
  | x -> x in
  refresh scheme.typ

let rec w e = function
| EConst (CBoolean _ as b) -> { e=TEConst b; t=TTBoolean }
| EConst (CNumber _ as n) -> { e=TEConst n; t=TTNumber }
| EConst (CString _ as s) -> { e=TEConst s; t=TTString }
| EOp (b, l) -> 
  begin 
    let ll = List.map (w e) l in
    let tt = List.map (fun x -> x.t) ll in
    let t = match b with
    | BEq | BNeq -> TTBoolean
    | BLt | BLeq | BGt | BGeq -> 
      List.iter (fun t -> unify_lt t TTNumber) tt; TTBoolean
    | BAdd -> 
      (try List.iter (fun t -> unify_lt t TTNumber) tt; TTNumber with
      | UnificationFailure _ ->  List.iter (unify_lt TTString) tt; TTString)
    | BSub | BMul | BDiv -> 
      List.iter (fun t -> unify_lt t TTNumber) tt; TTNumber
    | BAnd | BOr -> 
      List.iter (fun t -> unify_lt t TTBoolean) tt; TTBoolean in
    { e=TEOp (b, ll); t=t }
  end
| EVar v -> begin try { e=TEVar v; t=find v e }
  with Not_found -> Printf.eprintf "unbound variable %s\n" v; raise Error end
| EBlock b -> let bb, t = checkblock e b in 
  { e=TEBlock bb; t=t }
| EIf (l, eb) -> 
  let ll = List.map (fun (c, cb) -> (w e c, checkblock e cb)) l in
  let eeb, t = checkblock e eb in
  { e=TEIf (List.map (fun (cc, (ccb, cbt)) -> 
    unify_lt cc.t TTBoolean; unify cbt t; (cc, ccb)) ll, eeb); t=t }
| ECall (CVar f, l) -> begin match (try find f e with Not_found -> 
    Printf.eprintf "unbound variable %s\n" f; raise Error) with
  | TTArrow (lt, rt) -> let ll = List.map (w e) l in
    (try List.iter2 unify_lt lt (List.map (fun x -> x.t) ll)
    with Invalid_argument _ -> Printf.eprintf "incorrect number of arguments for %s\n" f; raise Error);
    { e=TECall (TCVar f, ll); t=rt } 
    (* ecall ccall !! *)
  | _ -> Printf.eprintf "%s should have an arrow type\n" f; raise Error end
| ECases (TVar ("List", [a]), c, ["empty", [], eb; "link", [x; y], lb])
| ECases (TVar ("List", [a]), c, ["link", [x; y], lb; "empty", [], eb]) -> 
  let aa = ttyp_of_typ a in
  let cc = w e c in
  unify_lt cc.t (TTList aa);
  let ee = (if x = "_" then (fun u -> u) else add false x aa false)
    ((if y = "_" then (fun u -> u) else add false y (TTList aa) false) e) in
  let (eeb, et), (llb, lt) = checkblock ee eb, checkblock ee lb in 
  unify et lt;
  { e=TECases (cc, ["empty", [], eeb; "link", [x; y], llb]); t=lt }
| ELam (pl, t, b) ->
  let tt = ttyp_of_typ t in
  let ppl = List.map (fun (p, pt) -> (p, ttyp_of_typ pt)) pl in
  let ee = List.fold_left (fun e (p, pt) -> add false p pt false e) e ppl in
  let bb, bt = checkblock ee b in
  unify_lt bt tt;
  { e=TELam (ppl, bb); t=TTArrow (List.map snd ppl, tt) }
| _ -> 
  Printf.eprintf "TODO"; raise Error
and checkblock e b = match b with
| [] -> assert false
| [s] -> let ss, _ = checkstmt e s in
  [ss], (match ss with TSExpr ex | TSAssign (_, ex) -> ex.t | _ -> TTNothing)
| s::bb -> let ss, ee = checkstmt e s in
  let l, t = checkblock ee bb in
  ss::l, t
and checkstmt e = function 
| SExpr ex -> TSExpr (w e ex), e
| SDecl (b, x, t, ex) -> 
  let s = w e ex in
  begin match t with 
  | None -> ()
  | Some a -> unify_lt s.t (ttyp_of_typ a)
  end;
  TSDecl (x, s), add false x s.t b e
| SAssign (x, ex) ->
  begin try let s = w e ex in 
    if snd (Smap.find x e.bindings) then begin
      unify_lt s.t (find x e); TSAssign (x, s), e
    end else begin Printf.eprintf "variable %s is read-only\n" x; raise Error
    end
  with Not_found -> 
    Printf.eprintf "unbound variable %s\n" x; raise Error end
| SFun (f, tl, (pl, t, b)) ->
  let ttl = List.map (fun v -> 
    if List.mem v ["Any"; "Nothing"; "Number"; "String"; "Boolean"; "List"] then
      (Printf.eprintf "name already taken %s\n" v; raise Error) 
    else TTVar (V.create ())) tl in
  let _ = ttl in
  let tt = ttyp_of_typ t in
  let ppl = List.map (fun (p, pt) -> (p, ttyp_of_typ pt)) pl in
  let ee = add false f (TTArrow (List.map snd ppl, tt)) false e in
  let eee = List.fold_left (fun e (p, pt) -> add false p pt false e) ee ppl in
  let bb, bt = checkblock eee b in
  unify_lt bt tt;
  TSFun (f, tl, (ppl, bb)), ee 
let check f = 
  try let b, _ = checkblock default f in b
  with 
  | Not_found -> 
    Printf.eprintf "not found at the end of check\n"; raise Error
  | UnificationFailure _ -> 
    Printf.eprintf "unif at the end of check\n"; raise Error