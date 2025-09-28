(* open Ast

(*
we are going to desugar: 
1. assignment operators
2. pass "this" to methods
3. think about method overloading...
*)

let gensym : string -> string =
  let c = ref 0 in
  fun (s : string) ->
    incr c;
    Printf.sprintf "_%s%d" s !c

let nl = no_loc
let nl_id : string -> exp node = fun id -> no_loc @@ Id id

let base_op = function
  | PluEq, _ -> Add
  | MinEq, _ -> Sub
  | TimEq, _ -> Mul
  | DivEq, _ -> Div
  | AtEq, _ -> At
  | PowEq, _ -> Pow
  | ModEq, _ -> Mod
  | _, loc ->
      let _, (s, e), _ = loc in
      failwith
      @@ Printf.sprintf "unsupported assignment operator at [%d, %d]" s e

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

let split (res : (stmt node list * exp node) list) =
  let rec aux lst stmts exps =
    match lst with
    | [] -> (stmts, exps)
    | (s, e) :: t -> aux t (s @ stmts) (e :: exps)
  in
  aux res [] []

let combine l1 l2 elem = List.rev_append l1 (elem :: l2)

let rec desugar_exp (e : exp node) : stmt node list * exp node =
  let dtmp = gensym "dtmp" in
  match e.elt with
  | Call (c, es) ->
      let cstmts, c' = desugar_exp c in
      let estmts, es' = split @@ List.map (fun v -> desugar_exp v) es in
      let res = Decl (dtmp, None, Call (c', es') |> nl, false) |> nl in
      (combine estmts cstmts res, nl_id dtmp)
  | Bop (bop, lhs, rhs) ->
      let lstmts, lhs' = desugar_exp lhs in
      let rstmts, rhs' = desugar_exp rhs in
      let res = Decl (dtmp, None, Bop (bop, lhs', rhs') |> nl, false) |> nl in
      (combine lstmts rstmts res, nl_id dtmp)
  | Uop (uop, o) ->
      let ostmts, o' = desugar_exp o in
      let res = Decl (dtmp, None, Uop (uop, o') |> nl, false) |> nl in
      (combine [] ostmts res, nl_id dtmp)
  | Index (c, i) ->
      let cstmts, cvar = desugar_exp c in
      let istmts, ivar = desugar_exp i in
      let res = Decl (dtmp, None, Index (cvar, ivar) |> nl, false) |> nl in
      (combine cstmts istmts res, nl_id dtmp)
  | Array es ->
      let estmts, es' = split @@ List.map (fun v -> desugar_exp v) es in
      let res = Decl (dtmp, None, Array es' |> nl, false) |> nl in
      (combine [] estmts res, nl_id dtmp)
  | _ -> ([], e) *)
(* TODO desugar range into some kind of struct or something *)
