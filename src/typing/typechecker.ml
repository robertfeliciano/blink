open Ast
open Tctxt

exception TypeError of string

let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

(* let check_list (l: exp node list) =
  let t = List.hd l in
  List.for_all (arg -> ) *)

(* let create_function_ctxt (prog: prog) (env: env) : env =
  env  *)



let typecheck_prog = function
| Gfdecl f -> Gfdecl f
| other -> other

let typecheck ast = List.map typecheck_prog ast