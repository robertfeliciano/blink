open Ast
open Tctxt

exception TypeError of string

let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

let typecheck_prog = function
| Gfdecl f -> Gfdecl f
| other -> other

let typecheck ast = 
  List.map typecheck_prog ast