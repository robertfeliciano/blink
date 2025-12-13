module Range = Util.Range

type 'a node = { elt : 'a; loc : Range.t }

let show_node show_elt { elt; loc } =
  Printf.sprintf "{ elt = %s; loc = %s }" (show_elt elt)
    (Range.string_of_range loc)

let no_loc x = { elt = x; loc = Range.norange }

type id = string
type sint = Ti8 | Ti16 | Ti32 | Ti64 | Ti128 [@@deriving show]
type uint = Tu8 | Tu16 | Tu32 | Tu64 | Tu128 [@@deriving show]
type float_ty = Tf32 | Tf64 [@@deriving show]
type int_ty = TSigned of sint | TUnsigned of uint [@@deriving show]

type ty = TBool | TInt of int_ty | TFloat of float_ty | TRef of ref_ty
(* | TOpt of ty *)

and ref_ty =
  | RString
  | RArray of ty * Z.t
  | RClass of id
  | RFun of ty list * ret_ty
  | RGeneric of string * ty list

and ret_ty = RetVoid | RetVal of ty

type unop = Neg | Not [@@deriving show]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | At
  | Mod
  | Pow
  | Shl
  | Lshr
  | Ashr
  | Eqeq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or
  | Xor (* | BXor 
  | BAnd 
  | BOr *)
[@@deriving show]

type aop =
  | Eq
  | PluEq
  | MinEq
  | TimEq
  | DivEq
  | AtEq
  | PowEq
  | ModEq
  | ShlEq
  | LShrEq
  | AShrEq
[@@deriving show]

type exp =
  | Bool of bool
  | Int of Z.t
  | Float of float
  | Str of string
  | Id of id
  | Proj of exp node * id
  | Call of exp node * exp node list
  | Bop of binop * exp node * exp node
  | Uop of unop * exp node
  | Index of exp node * exp node
  | Array of exp node list
  | Cast of exp node * ty
  | ObjInit of id node * (id node * exp node) list
  | Lambda of id list * block
  | TypedLambda of (id * ty) list * ret_ty * block
  | Null

and vdecl = id * ty option * exp node option * bool
and ldecl = id node * ref_ty option * exp node

and stmt =
  | Assn of exp node * aop * exp node
  | LambdaDecl of ldecl
  | Decl of vdecl (* includes whether it was declared as constant or not *)
  | Ret of exp node option
  | SCall of exp node * exp node list
  | If of exp node * block * block
  | ForEach of id node * exp node * block (* no step in for-each *)
  | For of
      (* iterator, start, stop, incl, step, body *)
      id node
      * (exp node * exp node * bool)
      * exp node option
      * block
  | While of exp node * block
  | Break
  | Continue

and block = stmt node list

type gdecl = { name : id; init : exp node }

type fdecl = {
  frtyp : ret_ty;
  fname : id;
  args : (ty * id) list;
  mutable body : block;
}

type field = { fieldName : id; ftyp : ty; init : exp node option }

type cdecl = {
  cname : id;
  impls : id list;
  fields : vdecl node list;
  methods : fdecl node list;
}

type program = Prog of fdecl node list * cdecl node list

(* Utility for indentation *)
let indent n = String.make (n * 2) ' '

(* Base show functions *)

let show_sint = function
  | Ti8 -> "i8"
  | Ti16 -> "i16"
  | Ti32 -> "i32"
  | Ti64 -> "i64"
  | Ti128 -> "i128"

let show_uint = function
  | Tu8 -> "u8"
  | Tu16 -> "u16"
  | Tu32 -> "u32"
  | Tu64 -> "u64"
  | Tu128 -> "u128"

let show_float_ty = function Tf32 -> "Tf32" | Tf64 -> "Tf64"

let show_int_ty = function
  | TSigned s -> Printf.sprintf "TSigned(%s)" (show_sint s)
  | TUnsigned u -> Printf.sprintf "TUnsigned(%s)" (show_uint u)

(* Type printers *)

let rec show_ref_ty ?(lvl = 0) = function
  | RString -> "RString"
  | RArray (t, sz) ->
      Printf.sprintf "RArray(%s, %s)"
        (show_ty ~lvl:(lvl + 1) t)
        (Z.to_string sz)
  | RClass cn -> Printf.sprintf "RClass(%s)" cn
  | RFun (args, ret) ->
      let args_s =
        String.concat "; " (List.map (fun a -> show_ty ~lvl:(lvl + 1) a) args)
      in
      Printf.sprintf "RFun([\n%s%s\n%s], %s)"
        (indent (lvl + 1))
        args_s (indent lvl)
        (show_ret_ty ~lvl:(lvl + 1) ret)
  | RGeneric _ -> failwith "generics not supported yet"

and show_ret_ty ?(lvl = 0) = function
  | RetVoid -> "RetVoid"
  | RetVal t -> Printf.sprintf "RetVal(%s)" (show_ty ~lvl:(lvl + 1) t)

and show_ty ?(lvl = 0) = function
  | TBool -> "TBool"
  | TInt it -> Printf.sprintf "TInt(%s)" (show_int_ty it)
  | TFloat ft -> Printf.sprintf "TFloat(%s)" (show_float_ty ft)
  | TRef rt -> Printf.sprintf "TRef(%s)" (show_ref_ty ~lvl:(lvl + 1) rt)
(* | TOpt t -> Printf.sprintf "TOpt(%s)" (show_ty t) *)

let rec show_exp ?(lvl = 0) = function
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | Int i -> Printf.sprintf "Int(%s)" (Z.to_string i)
  | Float f -> Printf.sprintf "Float(%f)" f
  | Null -> Printf.sprintf "Null"
  | Str s -> Printf.sprintf "Str(%S)" s
  | Id id -> Printf.sprintf "Id(%s)" id
  | Call (fn, args) ->
      let args_s =
        String.concat ";\n"
          (List.map
             (fun a ->
               Printf.sprintf "%s%s" (indent (lvl + 2)) (show_node show_exp a))
             args)
      in
      Printf.sprintf "%sCall(\n%s%s,\n%s[\n%s\n%s])" (indent lvl)
        (indent (lvl + 1))
        (show_node show_exp fn)
        (indent (lvl + 1))
        args_s (indent lvl)
  | Bop (op, lhs, rhs) ->
      Printf.sprintf "%sBop(%s,\n%s%s,\n%s%s)" (indent lvl) (show_binop op)
        (indent (lvl + 1))
        (show_node show_exp lhs)
        (indent (lvl + 1))
        (show_node show_exp rhs)
  | Uop (op, e) ->
      Printf.sprintf "%sUop(%s, %s)" (indent lvl) (show_unop op)
        (show_node show_exp e)
  | Index (arr, idx) ->
      Printf.sprintf "%sIndex(%s, %s)" (indent lvl) (show_node show_exp arr)
        (show_node show_exp idx)
  | Array elems ->
      let elems_s =
        String.concat ";\n"
          (List.map
             (fun e ->
               Printf.sprintf "%s%s" (indent (lvl + 2)) (show_node show_exp e))
             elems)
      in
      Printf.sprintf "%sArray([\n%s\n%s])" (indent lvl) elems_s (indent lvl)
  | Cast (e, t) ->
      Printf.sprintf "%sCast(%s, %s)" (indent lvl) (show_node show_exp e)
        (show_ty ~lvl:(lvl + 1) t)
  | Proj (e, i) ->
      Printf.sprintf "%sProj(%s, %s)" (indent lvl) (show_node show_exp e) i
  | ObjInit (cn, fields) ->
      let fields_s =
        String.concat ";\n"
          (List.map
             (fun (f, e) ->
               Printf.sprintf "%s%s = %s"
                 (indent (lvl + 2))
                 (show_node (fun x -> x) f)
                 (show_node show_exp e))
             fields)
      in
      Printf.sprintf "%sObjInit(%s, [\n%s\n%s])" (indent lvl)
        (show_node (fun x -> x) cn)
        fields_s (indent lvl)
  | Lambda (params, body) ->
      let params_s = String.concat ", " params in
      let body_s =
        String.concat ";\n" (List.map (show_node_stmt ~lvl:(lvl + 2)) body)
      in
      Printf.sprintf "%sLambda(params=[%s], body=[\n%s\n%s])" (indent lvl)
        params_s body_s (indent lvl)
  | TypedLambda (params, ret_ty, body) ->
      let params_s =
        String.concat "; "
          (List.map
             (fun (id, ty) -> Printf.sprintf "(%s : %s)" id (show_ty ty))
             params)
      in
      let body_s =
        String.concat ";\n" (List.map (show_node_stmt ~lvl:(lvl + 2)) body)
      in
      Printf.sprintf "%sTypedLambda(params=[%s]; ret=%s; body=[\n%s\n%s])"
        (indent lvl) params_s
        (show_ret_ty ~lvl:(lvl + 1) ret_ty)
        body_s (indent lvl)

(* Variable declarations *)

and show_vdecl ?(lvl = 0) (id, ty_opt, exp_opt, is_const) =
  Printf.sprintf "%sDecl{id=%s; ty=%s; const=%b;\n%sinit=%s}" (indent lvl) id
    (match ty_opt with Some ty -> show_ty ~lvl:(lvl + 1) ty | None -> "None")
    is_const
    (indent (lvl + 1))
    (if Option.is_some exp_opt then show_node show_exp (Option.get exp_opt)
     else "<default>")

and show_ldecl ?(lvl = 0) (id, ty_opt, exp) =
  Printf.sprintf "%sLambda{id=%s; ty=%s ;\n%sinit=%s}" (indent lvl)
    (show_node (fun x -> x) id)
    (match ty_opt with
    | Some ty -> show_ref_ty ~lvl:(lvl + 1) ty
    | None -> "None")
    (indent (lvl + 1))
    (show_node show_exp exp)
(* Statements *)

and show_stmt ?(lvl = 0) = function
  | Assn (lhs, op, rhs) ->
      Printf.sprintf "%sAssn(\n%s%s,\n%s%s,\n%s%s)" (indent lvl)
        (indent (lvl + 1))
        (show_node show_exp lhs)
        (indent (lvl + 1))
        (show_aop op)
        (indent (lvl + 1))
        (show_node show_exp rhs)
  | Decl v -> show_vdecl ~lvl v
  | LambdaDecl l -> show_ldecl ~lvl l
  | Ret exp_opt ->
      let e_s =
        match exp_opt with Some e -> show_node show_exp e | None -> "None"
      in
      Printf.sprintf "%sRet(%s)" (indent lvl) e_s
  | SCall (fn, args) ->
      let args_s =
        String.concat ";\n"
          (List.map
             (fun a ->
               Printf.sprintf "%s%s" (indent (lvl + 2)) (show_node show_exp a))
             args)
      in
      Printf.sprintf "%sSCall(%s, [\n%s\n%s])" (indent lvl)
        (show_node show_exp fn) args_s (indent lvl)
  | If (cond, then_block, else_block) ->
      Printf.sprintf "%sIf(\n%scond=%s,\n%sthen=[\n%s\n%s],\n%selse=[\n%s\n%s])"
        (indent lvl)
        (indent (lvl + 1))
        (show_node show_exp cond)
        (indent (lvl + 1))
        (String.concat ";\n"
           (List.map (show_node_stmt ~lvl:(lvl + 2)) then_block))
        (indent (lvl + 1))
        (indent (lvl + 1))
        (String.concat ";\n"
           (List.map (show_node_stmt ~lvl:(lvl + 2)) else_block))
        (indent lvl)
  | ForEach (id, start, body) ->
      Printf.sprintf "%sForEach(%s in %s) [\n%s\n%s]" (indent lvl)
        (show_node (fun x -> x) id)
        (show_node show_exp start)
        (String.concat ";\n" (List.map (show_node_stmt ~lvl:(lvl + 2)) body))
        (indent lvl)
  | For (id, (start, fin, incl), step_opt, body) ->
      Printf.sprintf "%sFor(%s from %s to %s incl=%b step=%s) [\n%s\n%s]"
        (indent lvl)
        (show_node (fun x -> x) id)
        (show_node show_exp start) (show_node show_exp fin) incl
        (match step_opt with Some s -> show_node show_exp s | None -> "None")
        (String.concat ";\n" (List.map (show_node_stmt ~lvl:(lvl + 2)) body))
        (indent lvl)
  | While (cond, body) ->
      Printf.sprintf "%sWhile(%s) [\n%s\n%s]" (indent lvl)
        (show_node show_exp cond)
        (String.concat ";\n" (List.map (show_node_stmt ~lvl:(lvl + 2)) body))
        (indent lvl)
  | Break -> Printf.sprintf "%sBreak" (indent lvl)
  | Continue -> Printf.sprintf "%sContinue" (indent lvl)

and show_node_stmt ?(lvl = 0) stmt_node =
  Printf.sprintf "%s;\n" (show_node (show_stmt ~lvl) stmt_node)

(* Function declarations *)

let show_fdecl ?(lvl = 0) { frtyp; fname; args; body } =
  let args_s =
    String.concat "; "
      (List.map
         (fun (ty, id) -> Printf.sprintf "(%s, %s)" (show_ty ty) id)
         args)
  in
  Printf.sprintf "%sfdecl{name=%s; ret=%s; args=[%s]; body=[\n%s\n%s]}"
    (indent lvl) fname
    (show_ret_ty ~lvl:(lvl + 1) frtyp)
    args_s
    (String.concat "" (List.map (show_node_stmt ~lvl:(lvl + 1)) body))
    (indent lvl)

(* Class and program printers *)

let show_field ?(lvl = 0) { fieldName; ftyp; init } =
  Printf.sprintf "%s%s: %s = %s" (indent lvl) fieldName
    (show_ty ~lvl:(lvl + 1) ftyp)
    (match init with Some e -> show_node show_exp e | None -> "None")

let show_cdecl ?(lvl = 0) { elt = { cname; impls; fields; methods }; loc = _ } =
  let fields_s =
    String.concat ";\n"
      (List.map (show_node (show_vdecl ~lvl:(lvl + 2))) fields)
  in
  let methods_s =
    String.concat "\n"
      (List.map (fun m -> show_fdecl ~lvl:(lvl + 2) m.elt) methods)
  in
  Printf.sprintf
    "%scdecl{name=%s; impls=[%s];\n%sfields=[\n%s\n%s];\n%smethods=[\n%s\n%s]}"
    (indent lvl) cname (String.concat ", " impls)
    (indent (lvl + 1))
    fields_s
    (indent (lvl + 1))
    (indent (lvl + 1))
    methods_s (indent lvl)

let show_decl ?(lvl = 0) d = show_fdecl ~lvl d.elt

let show_prog (Prog (fns, cns)) =
  let cns_s = String.concat "\n" (List.map (show_cdecl ~lvl:1) cns) in
  let fns_s = String.concat "\n" (List.map (show_decl ~lvl:1) fns) in
  Printf.sprintf "Program{\n%s\n%s\n}" cns_s fns_s
