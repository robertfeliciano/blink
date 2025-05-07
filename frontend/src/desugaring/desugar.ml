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
let tmp = 3+4
a = [1, 2, tmp]


let x = arr[funcCall(2, 3+4)];
becomes
let dtmp1 = 3+4;
let x = arr[funcCall(2, dtmp1)];
*)

let rec desugar_exp (e: exp) : stmt node list * exp node = 
  match e with 
  | Call(c, es) -> 
    let cstmts, c' = desugar_exp c.elt in
    let estmts, es' = List.split @@ List.map (fun v -> desugar_exp v.elt) es in 
    let dtmp = gensym "dtmp" in 
    let res = [Decl(dtmp, None, Call(c', es') |> nl, false) |> nl] in 
    List.flatten estmts @ cstmts @ res, nl_id dtmp
  | Bop(bop, lhs, rhs) -> 
    let lstmts, lhs' = desugar_exp lhs.elt in 
    let rstmts, rhs' = desugar_exp rhs.elt in 
    let dtmp = gensym "dtmp" in
    let res = [Decl(dtmp, None, Bop(bop, lhs', rhs') |> nl, false) |> nl] in 
    lstmts @ rstmts @ res, nl_id dtmp
  | Uop(uop, o) -> 
    let ostmts, o' = desugar_exp o.elt in 
    let dtmp = gensym "dtmp" in
    let res = [Decl(dtmp, None, Uop(uop, o') |> nl, false) |> nl] in
    ostmts @ res, nl_id dtmp
  | Index(c, i) -> 
    let cstmts, cvar = desugar_exp c.elt in 
    let istmts, ivar = desugar_exp i.elt in
    let dtmp = gensym "dtmp" in
    let res = [Decl(dtmp, None, Index(cvar, ivar) |> nl, false) |> nl] in 
    cstmts @ istmts @ res, nl_id dtmp
  | Array (es) -> 
    let estmts, es' = List.split @@ List.map (fun v -> desugar_exp v.elt) es in 
    let dtmp = gensym "dtmp" in
    let res = [Decl(dtmp, None, Array(es') |> nl, false) |> nl] in
    List.flatten estmts @ res, nl_id dtmp
  | _ -> [], nl e
  (* TODO desugar range into some kind of struct or something *)

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