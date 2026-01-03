let code _ = ()
(*

open Format
open X86_64
open Ast

(* phase 1 : allocation des variables *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = ident Smap.t

let rec alloc_expr (env: local_env) (fpcur: int) = function
  | PCst i ->
    Cst i, fpcur  

  | PVar x ->
    (try LVar (Smap.find x env)
    with Not_found -> if Hashtbl.mem genv x then GVar x
      else raise (VarUndef x)), fpcur

  | PBinop (o, e1, e2) ->
    let ee1, fp1 = alloc_expr env fpcur e1 in
    let ee2, fp2 = alloc_expr env fpcur e2 in
    Binop (o, ee1, ee2), max fp1 fp2

  | PLetin (x, e1, e2) ->
    let d = -fpcur - 8 in
    let ee1, fp1 = alloc_expr env fpcur e1 in
    let ee2, fp2 = alloc_expr (Smap.add x d env) (fpcur + 8) e2 in
    Letin (d, ee1, ee2), max fp1 fp2

  | PCall (f, l) ->
    let ll, fpmax =
      List.fold_left
        (fun (ll, fpmax) e ->
          let ee, fpmax' = alloc_expr env fpcur e in
          ee::ll, max fpmax fpmax') ([], fpcur) l
    in
    Call (f, ll), fpmax

let alloc_stmt = function
  | PSet (x, e) ->
    Hashtbl.replace genv x ();
    let ee, fp = alloc_expr Smap.empty 0 e in
    Set (x, ee, fp)

  | PFun (f, l, e) ->
    let env, _ = List.fold_left
      (fun (env, f) x -> 
        let ff = f + 8 in
        Smap.add x ff env, ff) 
      (Smap.empty, 8) l in
    let ee, fp = alloc_expr env 0 e in
    Fun (f, ee, fp)
    
  | PPrint e ->
    let e, fpmax = alloc_expr Smap.empty 0 e in
    Print (e, fpmax)

let alloc = List.map alloc_stmt

(******************************************************************************)
(* phase 2 : production de code *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec compile_expr = function
  | Cst i ->
      pushq (imm i)

  | LVar fp_x ->
      pushq (ind ~ofs:fp_x rbp)

  | GVar x ->
      pushq (lab x)

  | Binop (o, e1, e2)->
      compile_expr e1 ++
      compile_expr e2 ++
      popq rbx ++ popq rax ++
      (match o with
        | Add -> addq !%rbx !%rax
        | Sub -> subq !%rbx !%rax
        | Mul -> imulq !%rbx !%rax
        | Div -> cqto ++ idivq !%rbx) ++
       pushq !%rax

  | Letin (ofs, e1, e2) ->
      compile_expr e1 ++
      popq rax ++ movq !%rax (ind ~ofs rbp) ++
      compile_expr e2

  | Call (f, l) ->
    List.fold_left (fun code e -> code ++ compile_expr e) nop l ++
    call f ++ popn (8 * List.length l) ++ pushq !%rax

let compile_stmt (codefun, codemain) = function
  | Set (x, e, fpmax) ->
    let code =
      pushn fpmax ++
      compile_expr e ++
      popq rax ++ movq !%rax (lab x) ++
      popn fpmax
    in
    codefun, codemain ++ code

  | Fun (f, e, fpmax) ->
    let code =
      label f ++
      pushq !%rbp ++
      movq !%rsp !%rbp ++ pushn fpmax ++
      compile_expr e ++ popq rax ++
      popn fpmax ++ popq rbp ++ ret
    in
    codefun ++ code, codemain

  | Print (e, fpmax) ->
    let code =
      pushn fpmax ++
      compile_expr e ++
      popq rdi ++
      popn fpmax ++
      call "print_int"
    in
    codefun, codemain ++ code

let compile_program p ofile =
  let p = alloc p in
  Format.eprintf "%a@." print p;
  let codefun, code = List.fold_left compile_stmt (nop, nop) p in
  let p =
    { text =
        globl "main" ++ label "main" ++
        movq !%rsp !%rbp ++
        code ++
        movq (imm 0) !%rax ++ (* exit *)
        ret ++
        label "print_int" ++
        movq !%rdi !%rsi ++
        movq (ilab ".Sprint_int") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        ret ++
        codefun;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f

*)