(* open Ast
open Tctxt

exception TypeError of string

let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

let typecheck_prog = function
| Gfdecl f -> Gfdecl f
| other -> other

(* let rec typecheck_exp 

let typecheck_stmt (tc: Tctxt.t) (s: stmt node) (rty: ret_ty) : Tctxt.t = 
  let {elt=stmt; loc=stmt_loc} = s in
  match stmt with 
  | Assn ({elt=lhs; loc=lhs_loc}, aop, {elt=rhs; loc=rhs_loc}) -> 
    match lhs with 
    | _ -> 

    Assn ({elt=lhs; loc=lhs_loc}, aop=aop, e=e)
  | _ -> _ *)


let typecheck ast = 
  List.map typecheck_prog ast *)