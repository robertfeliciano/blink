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

and ref_ty =
  | RString
  | RArray of ty * Z.t
  | RClass of id
  | RFun of ty list * ret_ty

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
  | Eqeq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or
[@@deriving show]

type aop = Eq | PluEq | MinEq | TimEq | DivEq | AtEq | PowEq | ModEq
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

type vdecl = id * ty option * exp node * bool

type stmt =
  | Assn of exp node * aop * exp node
  | Decl of vdecl (* includes whether it was declared as constant or not *)
  | Ret of exp node option
  | SCall of exp node * exp node list
  | If of exp node * block * block
  | ForEach of id node * exp node * block (* no step in for-each *)
  | For of
      id node
      * (exp node * exp node * bool)
      * exp node option
      * block (* iterator, start, stop, incl, step, body *)
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
  fields : field node list;
  methods : fdecl node list;
}

type program = Prog of fdecl node list * cdecl node list [@@boxed]

external convert_caml_ast : program -> unit = "convert_caml_ast"

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

(* Manual show for ref_ty *)
let rec show_ref_ty = function
  | RString -> "RString"
  | RArray (t, sz) ->
      Printf.sprintf "RArray(%s, %s)" (show_ty t) (Z.to_string sz)
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
  | Int i -> Printf.sprintf "Int(%s)" (Z.to_string i)
  | Float f -> Printf.sprintf "Float(%f)" f
  | Str s -> Printf.sprintf "Str(%S)" s
  | Id id -> Printf.sprintf "Id(%s)" id
  | Call (fn, args) ->
      Printf.sprintf "Call(%s, [%s])" (show_node show_exp fn)
        (String.concat "; " (List.map (show_node show_exp) args))
  | Bop (op, lhs, rhs) ->
      Printf.sprintf "Bop(%s, %s, %s)" (show_binop op) (show_node show_exp lhs)
        (show_node show_exp rhs)
  | Uop (op, e) ->
      Printf.sprintf "Uop(%s, %s)" (show_unop op) (show_node show_exp e)
  | Index (arr, idx) ->
      Printf.sprintf "Index(%s, %s)" (show_node show_exp arr)
        (show_node show_exp idx)
  | Array elems ->
      Printf.sprintf "Array([%s])"
        (String.concat "; " (List.map (show_node show_exp) elems))
  | Cast (e, t) ->
      Printf.sprintf "Cast(%s, %s)" (show_node show_exp e) (show_ty t)
  | Proj (e, i) -> Printf.sprintf "Proj(%s, %s)" (show_node show_exp e) i
  | ObjInit (cn, fields) ->
      Printf.sprintf "ObjInit(%s, [%s])"
        (show_node (fun x -> x) cn)
        (String.concat "; "
           (List.map
              (fun (f, e) ->
                Printf.sprintf "%s=%s"
                  (show_node (fun x -> x) f)
                  (show_node show_exp e))
              fields))

let show_vdecl (id, ty_opt, exp, is_const) =
  Printf.sprintf "{ id = %s; ty = %s; exp = %s; is_const = %b }" id
    (match ty_opt with Some ty -> show_ty ty | None -> "None")
    (show_node show_exp exp) is_const

let rec show_stmt = function
  | Assn (lhs, op, rhs) ->
      Printf.sprintf "Assn(%s, %s, %s)" (show_node show_exp lhs) (show_aop op)
        (show_node show_exp rhs)
  | Decl v -> show_vdecl v
  | Ret exp_opt ->
      Printf.sprintf "Ret(%s)"
        (match exp_opt with
        | Some exp -> show_node show_exp exp
        | None -> "None")
  | SCall (fn, args) ->
      Printf.sprintf "SCall(%s, [%s])" (show_node show_exp fn)
        (String.concat "; " (List.map (show_node show_exp) args))
  | If (cond, then_block, else_block) ->
      Printf.sprintf "If(%s, [%s], [%s])" (show_node show_exp cond)
        (String.concat "; " (List.map show_node_stmt then_block))
        (String.concat "; " (List.map show_node_stmt else_block))
  | ForEach (id, start, body) ->
      Printf.sprintf "For(%s in %s, [%s])"
        (show_node (fun x -> x) id)
        (show_node show_exp start)
        (String.concat "; " (List.map show_node_stmt body))
  | For (id, (start, fin, incl), step_opt, body) ->
      Printf.sprintf "For(%s from %s to %s, incl=%b, step=%s, [%s])"
        (show_node (fun x -> x) id)
        (show_node show_exp start) (show_node show_exp fin) incl
        (match step_opt with Some s -> show_node show_exp s | None -> "None")
        (String.concat "; " (List.map show_node_stmt body))
  | While (cond, body) ->
      Printf.sprintf "While(%s, [%s])" (show_node show_exp cond)
        (String.concat "; " (List.map show_node_stmt body))
  | Break -> "Break"
  | Continue -> "Continue"

and show_node_stmt stmt_node = show_node show_stmt stmt_node ^ ";\n"

let show_fdecl { frtyp; fname; args; body } =
  Printf.sprintf "{ frtyp = %s; fname = %s; args = [%s]; body = [\n%s] }\n"
    (show_ret_ty frtyp) fname
    (String.concat "; "
       (List.map
          (fun (ty, id) -> Printf.sprintf "(%s, %s)" (show_ty ty) id)
          args))
    (String.concat "" (List.map show_node_stmt body))

let show_decl d = show_fdecl d.elt

let show_prog (p : program) =
  let (Prog (fns, _cns)) = p in
  let aux fn s = s ^ show_decl fn ^ "\n" in
  List.fold_right aux fns "\n"
