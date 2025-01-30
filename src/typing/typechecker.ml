(* open Ast
open Tctxt

exception TypeError of string

let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

let typecheck_prog = function
| Gfdecl f -> Gfdecl f
| other -> other

let signed_int_hierarchy: sint list = [Ti8; Ti16; Ti32; Ti64; Ti128]
let unsigned_int_hierarchy: uint list = [Tu8; Tu16; Tu32; Tu64; Tu128]

let float_hierarchy: float_ty list = [Tf32; Tf64]



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