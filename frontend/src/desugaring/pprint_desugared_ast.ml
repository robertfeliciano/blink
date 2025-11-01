open Desugared_ast

(* Utility for indentation *)
let indent n = String.make (n * 2) ' '

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

let show_unop = function Neg -> "Neg" | Not -> "Not"

let show_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | At -> "At"
  | Mod -> "Mod"
  | Pow -> "Pow"
  | Eqeq -> "Eqeq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Lte -> "Lte"
  | Gt -> "Gt"
  | Gte -> "Gte"
  | And -> "And"
  | Or -> "Or"

(* Expressions *)
let rec show_exp ?(lvl = 0) = function
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | Int (i, ity) ->
      Printf.sprintf "Int(%s, %s)" (Z.to_string i) (show_int_ty ity)
  | Float (f, fty) -> Printf.sprintf "Float(%f, %s)" f (show_float_ty fty)
  | Str s -> Printf.sprintf "Str(%S)" s
  | Id id -> Printf.sprintf "Id(%s)" id
  | Call (fn, args, ty) ->
      let args_s =
        String.concat ", " (List.map (fun e -> show_exp ~lvl:(lvl + 1) e) args)
      in
      Printf.sprintf "Call(%s, [%s], %s)"
        (show_exp ~lvl:(lvl + 1) fn)
        args_s (show_ty ty)
  | Bop (op, lhs, rhs, ty) ->
      Printf.sprintf "Bop(%s,\n%s%s,\n%s%s,\n%s%s)" (show_binop op)
        (indent (lvl + 1))
        (show_exp ~lvl:(lvl + 1) lhs)
        (indent (lvl + 1))
        (show_exp ~lvl:(lvl + 1) rhs)
        (indent (lvl + 1))
        (show_ty ty)
  | Uop (op, e, ty) ->
      Printf.sprintf "Uop(%s, %s, %s)" (show_unop op)
        (show_exp ~lvl:(lvl + 1) e)
        (show_ty ty)
  | Index (arr, idx, ty) ->
      Printf.sprintf "Index(%s, %s, %s)"
        (show_exp ~lvl:(lvl + 1) arr)
        (show_exp ~lvl:(lvl + 1) idx)
        (show_ty ty)
  | Array (elems, ty, sz) ->
      let elems_s =
        String.concat ", " (List.map (fun e -> show_exp ~lvl:(lvl + 1) e) elems)
      in
      Printf.sprintf "Array([%s], %s; %Ld)" elems_s (show_ty ty) sz
  | Cast (e, t) ->
      Printf.sprintf "Cast(%s, %s)" (show_exp ~lvl:(lvl + 1) e) (show_ty t)
  | Proj (e, i) -> Printf.sprintf "Proj(%s, %s)" (show_exp ~lvl:(lvl + 1) e) i
  | ObjInit (cn, fields) ->
      let fs =
        String.concat ";\n"
          (List.map
             (fun (f, e) ->
               Printf.sprintf "%s%s=%s"
                 (indent (lvl + 1))
                 f
                 (show_exp ~lvl:(lvl + 2) e))
             fields)
      in
      Printf.sprintf "ObjInit(%s, [\n%s\n%s])" cn fs (indent lvl)

(* Declarations *)
let show_vdecl ?(lvl = 0) (id, ty, e, is_const) =
  Printf.sprintf "%sDecl{id=%s; ty=%s; const=%b;\n%sinit=%s}" (indent lvl) id
    (show_ty ty) is_const
    (indent (lvl + 1))
    (show_exp ~lvl:(lvl + 1) e)

(* Statements *)
let rec show_stmt ?(lvl = 0) = function
  | Assn (lhs, rhs, t) ->
      Printf.sprintf "%sAssn(\n%s%s,\n%s%s,\n%s%s)" (indent lvl)
        (indent (lvl + 1))
        (show_exp ~lvl:(lvl + 1) lhs)
        (indent (lvl + 1))
        (show_exp ~lvl:(lvl + 1) rhs)
        (indent (lvl + 1))
        (show_ty t)
  | Decl v -> show_vdecl ~lvl v
  | Ret eo ->
      let e_s =
        match eo with None -> "None" | Some e -> show_exp ~lvl:(lvl + 1) e
      in
      Printf.sprintf "%sRet(%s)" (indent lvl) e_s
  | SCall (fn, args) ->
      let args_s =
        String.concat ", " (List.map (fun e -> show_exp ~lvl:(lvl + 1) e) args)
      in
      Printf.sprintf "%sSCall(%s, [%s])" (indent lvl)
        (show_exp ~lvl:(lvl + 1) fn)
        args_s
  | If (cond, tblock, eblock) ->
      Printf.sprintf "%sIf(\n%scond=%s,\n%sthen=[\n%s\n%s],\n%selse=[\n%s\n%s])"
        (indent lvl)
        (indent (lvl + 1))
        (show_exp ~lvl:(lvl + 1) cond)
        (indent (lvl + 1))
        (String.concat ";\n"
           (List.map (fun s -> show_stmt ~lvl:(lvl + 2) s) tblock))
        (indent (lvl + 1))
        (indent (lvl + 1))
        (String.concat ";\n"
           (List.map (fun s -> show_stmt ~lvl:(lvl + 2) s) eblock))
        (indent lvl)
  | ForEach (id, iter, iter_ty, body) ->
      Printf.sprintf "%sForEach(%s in %s : %s [\n%s\n%s])" (indent lvl) id
        (show_exp ~lvl:(lvl + 1) iter)
        (show_ty iter_ty)
        (String.concat ";\n"
           (List.map (fun s -> show_stmt ~lvl:(lvl + 2) s) body))
        (indent lvl)
  | For (id, start, stop, incl, step, step_ty, body) ->
      Printf.sprintf "%sFor(%s = %s to %s incl=%b step=%s : %s [\n%s\n%s])"
        (indent lvl) id
        (show_exp ~lvl:(lvl + 1) start)
        (show_exp ~lvl:(lvl + 1) stop)
        incl
        (show_exp ~lvl:(lvl + 1) step)
        (show_ty step_ty)
        (String.concat ";\n"
           (List.map (fun s -> show_stmt ~lvl:(lvl + 2) s) body))
        (indent lvl)
  | While (cond, body) ->
      Printf.sprintf "%sWhile(%s) [\n%s\n%s]" (indent lvl)
        (show_exp ~lvl:(lvl + 1) cond)
        (String.concat ";\n"
           (List.map (fun s -> show_stmt ~lvl:(lvl + 2) s) body))
        (indent lvl)
  | Break -> Printf.sprintf "%sBreak" (indent lvl)
  | Continue -> Printf.sprintf "%sContinue" (indent lvl)

let show_block ?(lvl = 0) b =
  String.concat ";\n" (List.map (fun s -> show_stmt ~lvl s) b)

(* Function, class, and program printers *)

let show_fdecl ?(lvl = 0) { frtyp; fname; args; body } =
  let args_s =
    String.concat "; "
      (List.map
         (fun (ty, id) -> Printf.sprintf "(%s, %s)" (show_ty ty) id)
         args)
  in
  Printf.sprintf "%sfdecl{name=%s; ret=%s; args=[%s]; body=[\n%s\n%s]}"
    (indent lvl) fname (show_ret_ty frtyp) args_s
    (show_block ~lvl:(lvl + 1) body)
    (indent lvl)

let show_field ?(lvl = 0) { prelude; fieldName; ftyp; init } =
  Printf.sprintf "%s\n%s%s: %s = %s" (indent lvl) (show_block ~lvl prelude)
    fieldName (show_ty ftyp)
    (show_exp ~lvl:(lvl + 1) init)

let show_cdecl ?(lvl = 0) { cname; fields } =
  let fields_s =
    String.concat ";\n" (List.map (fun f -> show_field ~lvl:(lvl + 1) f) fields)
  in
  Printf.sprintf "%scdecl{name=%s; \n%sfields=[\n%s];\n}"
    (indent (lvl + 1))
    cname fields_s
    (indent (lvl + 1))

let show_desugared_program (Prog (fns, cns)) =
  let cns_s = String.concat "\n" (List.map (show_cdecl ~lvl:1) cns) in
  let fns_s = String.concat "\n" (List.map (show_fdecl ~lvl:1) fns) in
  Printf.sprintf "Program{\nClasses{\n%s\n}\nFunctions{\n%s\n}}" cns_s fns_s
