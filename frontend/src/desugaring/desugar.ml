open Ast

let gensym: string -> string = 
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

let nl = no_loc
let nl_id: string -> exp node = fun id -> no_loc @@ Id id

let base_op = function
| (PluEq, _) -> Add
| (MinEq, _) -> Sub
| (TimEq, _) -> Mul
| (DivEq, _) -> Div
| (AtEq, _)  -> At
| (PowEq, _) -> Pow
| (ModEq, _) -> Mod
| (_, loc) -> 
  let (_, (s, e), _) = loc in 
  failwith @@ Printf.sprintf "unsupported assignment operator at [%d, %d]" s e

(* 
for desugaring exp like in x = x*3+2
could return [new lines, final_result_var]
so could use the var in next statements
*)


(* x = (x*3) + 2 turns into
tmp1 = x * 3
x = tmp1 + 2

a = [1, 2, 3+4] becomes
tmp = e+4
a = [1, 2, tmp]
*)

let rec desugar_exp (e: exp) : stmt node list * exp node = 
  match e with 
  (* | Call(c, es) -> 
    let cstmts, c' = desugar_exp c.elt *)
  | Bop(bop, lhs, rhs) -> 
    let lstmts, lhs' = desugar_exp lhs.elt in 
    let rstmts, rhs' = desugar_exp rhs.elt in 
    let dtmp = nl_id @@ gensym "dtmp" in
    let res = [nl @@ Assn(dtmp, Eq, nl @@ Bop(bop, lhs', rhs'))] in
    (lstmts @ rstmts @ res, dtmp)
  | Uop(uop, o) -> 
    let ostmts, o' = desugar_exp o.elt in 
    let dtmp = nl_id @@ gensym "dtmp" in
    let res = [nl @@ Assn(dtmp, Eq, nl @@ Uop(uop, o'))] in 
    (ostmts @ res, dtmp)
  | Index(c, i) -> 
    let cstmts, cvar = desugar_exp c.elt in 
    let istmts, ivar = desugar_exp i.elt in
    let dtmp = nl_id @@ gensym "dtmp" in
    let res = [nl @@ Assn(dtmp, Eq, nl @@ Index(cvar, ivar))] in
    (cstmts @ istmts @ res, dtmp)
  | Array (es) -> 
    let estmts, es' = List.split @@ List.map (fun v -> desugar_exp v.elt) es in 
    let dtmp = nl_id @@ gensym "dtmp" in
    let res = [nl @@ Assn(dtmp, Eq, nl @@ Array(es'))] in 
    List.flatten estmts @ res, dtmp
  | _ -> [], nl e

(* let rec desugar_stmt (s: stmt node) : stmt node list = 
  let node_creator elem = {elt = elem; loc = s.loc} in 
  match s.elt with 
  | Assn(lhs, op, rhs) ->
    (* let rhs' = rhs in 
    if op <> Eq then
      let base_op = base_op (op, s.loc) in 
      let rhs' = no_loc @@ Bop(base_op, lhs, rhs) 
    else 
      rhs' = rhs
    in
    [node_creator Assn(lhs, Eq, rhs')] *)

  
  | While(cond, body) -> 
    let body' = List.concat (List.map desugar_stmt body) in 
    [node_creator @@ While(cond, body')] *)

(* let desugar_prog = function 
  | Gfdecl f -> Gfdecl (desugar_func f)

let desugar prog = List.map desugar_prog prog *)