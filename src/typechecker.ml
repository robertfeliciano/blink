open Ast
open Tctxt

let typ_of_binop = function 
  | Add | Sub | Mul | Div | At | Mod | Pow -> (TInt, TInt, TInt)
  | Eqeq | Neq -> failwith "typ_of_binop called on == or !="
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

let typ_of_unop = function 
  | Neg -> (TInt, TInt)
  | Not -> (TBool, TBool)

let typecheck_prog = function
  | Gfdecl f -> Gfdecl f
  | other -> other

let typecheck ast = List.map typecheck_prog ast

(* let create_function_ctxt (prog: prog) (env: env) : env = 
  List.fold_left
  (fun e decl -> 
    match decl with
    | Gfdecl ({elt=f} as l) -> 
      if Hashtbl.find
  )
  env prog *)