open Typed_ast

(* Show functions for basic types *)

let show_sint = function
  | Ti8 -> "Ti8"
  | Ti16 -> "Ti16"
  | Ti32 -> "Ti32"
  | Ti64 -> "Ti64"
  | Ti128 -> "Ti128"

let show_uint = function
  | Tu8 -> "Tu8"
  | Tu16 -> "Tu16"
  | Tu32 -> "Tu32"
  | Tu64 -> "Tu64"
  | Tu128 -> "Tu128"

let show_float_ty = function Tf32 -> "Tf32" | Tf64 -> "Tf64"

let show_int_ty = function
  | TSigned s -> Printf.sprintf "TSigned(%s)" (show_sint s)
  | TUnsigned u -> Printf.sprintf "TUnsigned(%s)" (show_uint u)

(* Manual show for ref_ty *)
let rec show_ref_ty = function
  | RString -> "RString"
  | RArray (t, sz) -> Printf.sprintf "RArray(%s, %Ld)" (show_ty t) sz
  | RRange (t1, t2) -> Printf.sprintf "RRange(%s, %s)" (show_ty t1) (show_ty t2)
  | RClass cn -> Printf.sprintf "RClass(%s)" cn
  | RFun (args, ret) ->
      let args_s = String.concat "; " (List.map show_ty args) in
      Printf.sprintf "RFun([%s], %s)" args_s (show_ret_ty ret)

and show_ret_ty = function
  | RetVoid -> "RetVoid"
  | RetVal t -> Printf.sprintf "RetVal(%s)" (show_ty t)

and show_ty = function
  | TBool -> "TBool"
  | TInt it -> Printf.sprintf "TInt(%s)" (show_int_ty it)
  | TFloat ft -> Printf.sprintf "TFloat(%s)" (show_float_ty ft)
  | TRef rt -> Printf.sprintf "TRef(%s)" (show_ref_ty rt)

let rec show_exp = function
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | Int (i, ity) ->
      Printf.sprintf "Int(%s, %s)" (Z.to_string i) (show_int_ty ity)
  | Float (f, fty) -> Printf.sprintf "Float(%f, %s)" f (show_float_ty fty)
  | Str s -> Printf.sprintf "Str(%S)" s
  | Id id -> Printf.sprintf "Id(%s)" id
  | Call (fn, args, ty) ->
      Printf.sprintf "Call(%s, [%s], %s)" (show_exp fn)
        (String.concat "; " (List.map show_exp args))
        (show_ty ty)
  | Bop (op, lhs, rhs, ty) ->
      Printf.sprintf "Bop(%s, %s, %s, %s)" (show_binop op) (show_exp lhs)
        (show_exp rhs) (show_ty ty)
  | Uop (op, e, ty) ->
      Printf.sprintf "Uop(%s, %s, %s)" (show_unop op) (show_exp e) (show_ty ty)
  | Index (arr, idx, ty) ->
      Printf.sprintf "Index(%s, %s, %s)" (show_exp arr) (show_exp idx)
        (show_ty ty)
  | Array (elems, ty, sz) ->
      Printf.sprintf "Array([%s], %s;%Ld)"
        (String.concat "; " (List.map show_exp elems))
        (show_ty ty) sz
  | Cast (e, t) -> Printf.sprintf "Cast(%s, %s)" (show_exp e) (show_ty t)
  | Proj (e, i) -> Printf.sprintf "Proj(%s, %s)" (show_exp e) i
  | ObjInit (cn, fields) ->
      Printf.sprintf "ObjInit(%s, [%s])" cn
        (String.concat "; "
           (List.map
              (fun (f, e) -> Printf.sprintf "%s=%s" f (show_exp e))
              fields))

let show_vdecl (id, ty, e, is_const) =
  Printf.sprintf "Decl{id=%s; ty=%s; exp=%s; const=%b}" id (show_ty ty)
    (show_exp e) is_const

let rec show_stmt = function
  | Assn (lhs, op, rhs, t) ->
      Printf.sprintf "Assn(%s, %s, %s, %s)" (show_exp lhs) (show_aop op)
        (show_exp rhs) (show_ty t)
  | Decl v -> show_vdecl v
  | Ret eo ->
      Printf.sprintf "Ret(%s)"
        (match eo with None -> "None" | Some e -> show_exp e)
  | SCall (fn, args) ->
      Printf.sprintf "SCall(%s, [%s])" (show_exp fn)
        (String.concat "; " (List.map show_exp args))
  | If (cond, tblock, eblock) ->
      Printf.sprintf "If(%s, [%s], [%s])" (show_exp cond)
        (String.concat "; " (List.map show_stmt tblock))
        (String.concat "; " (List.map show_stmt eblock))
  | For (id, start, stop, body) ->
      Printf.sprintf "For(%s, %s, %s, [%s])" id (show_exp start) (show_exp stop)
        (String.concat "; " (List.map show_stmt body))
  | While (cond, body) ->
      Printf.sprintf "While(%s, [%s])" (show_exp cond)
        (String.concat "; " (List.map show_stmt body))
  | Break -> "Break"
  | Continue -> "Continue"

let show_block b = String.concat ";\n" (List.map show_stmt b)

let show_fdecl { frtyp; fname; args; body } =
  Printf.sprintf "fdecl{ret=%s; name=%s; args=[%s]; body=[\n%s\n]}"
    (show_ret_ty frtyp) fname
    (String.concat "; "
       (List.map
          (fun (ty, id) -> Printf.sprintf "(%s,%s)" (show_ty ty) id)
          args))
    (show_block body)

let show_typed_program (Prog (fns, _)) =
  String.concat "\n" (List.map show_fdecl fns)
