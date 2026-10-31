module Range = Util.Range

type 'a node = { elt : 'a; loc : Range.t }

exception ParserError of Lexing.position * string
exception InvalidObjectType of Range.t

let show_node show_elt { elt; loc } =
  Printf.sprintf "{ elt = %s; loc = %s }" (show_elt elt)
    (Range.string_of_range loc)

let no_loc x = { elt = x; loc = Range.norange }

type id = string
type qualified_name = { qualifiers : id node list; name : id node }

let unqualified_name name = { elt = { qualifiers = []; name }; loc = name.loc }
let name_components name = name.elt.qualifiers @ [ name.elt.name ]

let show_qualified_name name =
  String.concat "." (List.map (fun part -> part.elt) (name_components name))

let unqualified_id name =
  match name.elt.qualifiers with [] -> Some name.elt.name.elt | _ -> None

type sint = Ti8 | Ti16 | Ti32 | Ti64 | Ti128 [@@deriving show]
type uint = Tu8 | Tu16 | Tu32 | Tu64 | Tu128 [@@deriving show]
type float_ty = Tf32 | Tf64 [@@deriving show]
type int_ty = TSigned of sint | TUnsigned of uint [@@deriving show]

type ty =
  | TBool
  | TInt of int_ty
  | TFloat of float_ty
  | TRef of ref_ty
  | TParam of id node
(* | TOpt of ty *)

and ref_ty =
  | RString
  | RArray of ty * Z.t
  | RClass of qualified_name node
  | RFun of ty list * ret_ty
  | RGeneric of qualified_name node * ty list

and ret_ty = RetVoid | RetVal of ty

type unop = Neg | Not | BNeg [@@deriving show]

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
  | Xor
  | BXor
  | BAnd
  | BOr
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
  | BXorEq
  | BAndEq
  | BOrEq
[@@deriving show]

type exp =
  | Bool of bool
  | Int of Z.t
  | Float of float
  | Str of string
  | Id of id
  | Proj of exp node * id
  | Call of exp node * exp node list
  (* | StaticCall of id node * id node * exp node
  TODO static class methods and fields + accesses
  *)
  | Bop of binop * exp node * exp node
  | Uop of unop * exp node
  | Index of exp node * exp node
  | Array of exp node list * bool (* elements, on_heap *)
  | Cast of exp node * ty
  | ObjInit of ref_ty node * (id node * exp node) list
  | Lambda of exp node list * id list * block (* scope, args, body *)
  | TypedLambda of
      exp node list
      * (id * ty) list
      * ret_ty
      * block (* scope, args+types, ret type, body *)
  | Null
  | Conditional of exp node * exp node * exp node

and vdecl = id * ty option * exp node option * bool

and stmt =
  | Assn of exp node * aop * exp node
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
  | Free of exp node list

and block = stmt node list

type gdecl = {
  gname : id;
  gtyp : ty option;
  ginit : exp node option;
  gconst : bool;
}

type annotation = id node * exp node list option

let has_annotation name annotations =
  List.exists (fun (id, _) -> id.elt = name) annotations

type type_param = { pname : id node; bound : ty node option }

let map_type_params ty params =
  List.map
    (fun p ->
      { p with bound = Option.map (fun n -> { n with elt = ty n.elt }) p.bound })
    params

type fdecl = {
  type_params : type_param list;
  annotations : annotation list;
  frtyp : ret_ty;
  fname : id;
  args : (ty * id) list;
  body : block;
  inline : bool;
}

type field = { fieldName : id; ftyp : ty; init : exp node option }

type cdecl = {
  cparams : type_param list;
  annotations : annotation list;
  cname : id;
  impls : ty node list;
  fields : vdecl node list;
  methods : fdecl node list;
}

type proto = {
  type_params : type_param list;
  annotations : annotation list;
  frtyp : ret_ty;
  fname : id;
  args : (ty * id) list;
}

type ifield = { fieldName : id; ftyp : ty; fconst : bool }

type idecl = {
  iname : id;
  iparams : type_param list;
  protos : proto node list;
  fields : ifield node list;
}

type import_selection =
  | Module_import of id node option
  | Named_import of id node list

type import = { path : qualified_name node; selection : import_selection }

type top_level_decl =
  | Function of fdecl node
  | Class of cdecl node
  | Prototype of proto node
  | Interface of idecl node
  | Global of gdecl node

type top_level = { declaration : top_level_decl; export_loc : Range.t option }
type program = Prog of import node list * top_level node list

(* Preserve source order in the AST; consumers that collect headers by kind
   share this partition rather than duplicating the classification rule. *)
let partition_declarations (Prog (_, declarations)) =
  List.fold_right
    (fun { elt = { declaration; _ }; _ }
         (functions, classes, prototypes, globals) ->
      match declaration with
      | Function fn -> (fn :: functions, classes, prototypes, globals)
      | Class cn -> (functions, cn :: classes, prototypes, globals)
      | Prototype pn -> (functions, classes, pn :: prototypes, globals)
      | Interface _ -> (functions, classes, prototypes, globals)
      | Global gn -> (functions, classes, prototypes, gn :: globals))
    declarations ([], [], [], [])

(* Type parameters are bound before module resolution. Share this traversal
   with specialization so annotations inside lambdas and local declarations
   follow the same substitution as the function signature. *)
let type_mappers ty =
  let ret_ty = function RetVoid -> RetVoid | RetVal t -> RetVal (ty t) in
  let rec exp en =
    let elt =
      match en.elt with
      | Call (f, es) -> Call (exp f, List.map exp es)
      | Proj (e, i) -> Proj (exp e, i)
      | Bop (op, e1, e2) -> Bop (op, exp e1, exp e2)
      | Uop (op, e) -> Uop (op, exp e)
      | Index (e1, e2) -> Index (exp e1, exp e2)
      | Array (es, on_heap) -> Array (List.map exp es, on_heap)
      | Cast (e, t) -> Cast (exp e, ty t)
      | ObjInit (cn, fields) ->
          let cn =
            match ty (TRef cn.elt) with
            | TRef ref_ty -> { cn with elt = ref_ty }
            | _ -> raise (InvalidObjectType cn.loc)
          in
          ObjInit (cn, List.map (fun (i, e) -> (i, exp e)) fields)
      | Lambda (scope, args, body) ->
          Lambda (List.map exp scope, args, block body)
      | TypedLambda (scope, args, ret, body) ->
          TypedLambda
            ( List.map exp scope,
              List.map (fun (i, t) -> (i, ty t)) args,
              ret_ty ret,
              block body )
      | Conditional (e1, e2, e3) -> Conditional (exp e1, exp e2, exp e3)
      | (Bool _ | Int _ | Float _ | Str _ | Id _ | Null) as e -> e
    in
    { en with elt }
  and block body = List.map stmt body
  and stmt sn =
    let elt =
      match sn.elt with
      | Decl (i, t, e, const) ->
          Decl (i, Option.map ty t, Option.map exp e, const)
      | Assn (e1, op, e2) -> Assn (exp e1, op, exp e2)
      | Ret e -> Ret (Option.map exp e)
      | SCall (f, es) -> SCall (exp f, List.map exp es)
      | If (e, b1, b2) -> If (exp e, block b1, block b2)
      | ForEach (i, e, b) -> ForEach (i, exp e, block b)
      | For (i, (e1, e2, incl), step, b) ->
          For (i, (exp e1, exp e2, incl), Option.map exp step, block b)
      | While (e, b) -> While (exp e, block b)
      | Free es -> Free (List.map exp es)
      | (Break | Continue) as s -> s
    in
    { sn with elt }
  in
  (exp, block)

let map_exp_types ty = fst (type_mappers ty)

let map_vdecl_types ty (name, annotation, init, const) =
  (name, Option.map ty annotation, Option.map (map_exp_types ty) init, const)

let map_fdecl_types ty (fn : fdecl node) =
  let exp, block = type_mappers ty in
  let ret_ty = function RetVoid -> RetVoid | RetVal t -> RetVal (ty t) in
  {
    fn with
    elt =
      {
        fn.elt with
        type_params = map_type_params ty fn.elt.type_params;
        args = List.map (fun (t, i) -> (ty t, i)) fn.elt.args;
        frtyp = ret_ty fn.elt.frtyp;
        annotations =
          List.map
            (fun (i, es) -> (i, Option.map (List.map exp) es))
            fn.elt.annotations;
        body = block fn.elt.body;
      };
  }

let map_cdecl_types ty (cn : cdecl node) =
  {
    cn with
    elt =
      {
        cn.elt with
        cparams = map_type_params ty cn.elt.cparams;
        impls = List.map (fun n -> { n with elt = ty n.elt }) cn.elt.impls;
        fields =
          List.map
            (fun field -> { field with elt = map_vdecl_types ty field.elt })
            cn.elt.fields;
        methods = List.map (map_fdecl_types ty) cn.elt.methods;
        annotations =
          List.map
            (fun (name, args) ->
              (name, Option.map (List.map (map_exp_types ty)) args))
            cn.elt.annotations;
      };
  }

let rec exists_type_param predicate = function
  | TParam p -> predicate p
  | TRef (RArray (ty, _)) -> exists_type_param predicate ty
  | TRef (RGeneric (_, args)) -> List.exists (exists_type_param predicate) args
  | TRef (RFun (args, ret)) -> (
      List.exists (exists_type_param predicate) args
      ||
      match ret with
      | RetVoid -> false
      | RetVal ty -> exists_type_param predicate ty)
  | _ -> false

let rec substitute_type_params lookup = function
  | TParam p -> lookup p
  | TRef (RGeneric (name, args)) ->
      TRef (RGeneric (name, List.map (substitute_type_params lookup) args))
  | TRef (RArray (ty, size)) ->
      TRef (RArray (substitute_type_params lookup ty, size))
  | TRef (RFun (args, ret)) ->
      let ret =
        match ret with
        | RetVoid -> RetVoid
        | RetVal ty -> RetVal (substitute_type_params lookup ty)
      in
      TRef (RFun (List.map (substitute_type_params lookup) args, ret))
  | ty -> ty

let rec bind_type_params params = function
  | TRef (RClass cn) as t -> (
      match unqualified_id cn with
      | Some i when List.exists (fun p -> p.pname.elt = i) params ->
          TParam cn.elt.name
      | _ -> t)
  | TRef (RGeneric (name, args)) ->
      TRef (RGeneric (name, List.map (bind_type_params params) args))
  | TRef (RArray (t, sz)) -> TRef (RArray (bind_type_params params t, sz))
  | TRef (RFun (args, ret)) ->
      let ret =
        match ret with
        | RetVoid -> RetVoid
        | RetVal t -> RetVal (bind_type_params params t)
      in
      TRef (RFun (List.map (bind_type_params params) args, ret))
  | t -> t

let bind_fdecl_params (fn : fdecl node) =
  map_fdecl_types (bind_type_params fn.elt.type_params) fn

let map_proto_types ty (pn : proto node) =
  {
    pn with
    elt =
      {
        pn.elt with
        type_params = map_type_params ty pn.elt.type_params;
        args = List.map (fun (t, i) -> (ty t, i)) pn.elt.args;
        frtyp =
          (match pn.elt.frtyp with
          | RetVoid -> RetVoid
          | RetVal t -> RetVal (ty t));
        annotations =
          List.map
            (fun (name, args) ->
              (name, Option.map (List.map (map_exp_types ty)) args))
            pn.elt.annotations;
      };
  }

let map_idecl_types ty (interface : idecl node) =
  {
    interface with
    elt =
      {
        interface.elt with
        iparams = map_type_params ty interface.elt.iparams;
        fields =
          List.map
            (fun (field : ifield node) ->
              { field with elt = { field.elt with ftyp = ty field.elt.ftyp } })
            interface.elt.fields;
        protos = List.map (map_proto_types ty) interface.elt.protos;
      };
  }

let bind_proto_params (pn : proto node) =
  map_proto_types (bind_type_params pn.elt.type_params) pn

let interface_declarations (Prog (_, declarations)) =
  List.filter_map
    (fun item ->
      match item.elt.declaration with
      | Interface value -> Some value
      | _ -> None)
    declarations

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
  | RClass cn -> Printf.sprintf "RClass(%s)" (show_qualified_name cn)
  | RFun (args, ret) ->
      let args_s =
        String.concat "; " (List.map (fun a -> show_ty ~lvl:(lvl + 1) a) args)
      in
      Printf.sprintf "RFun([\n%s%s\n%s], %s)"
        (indent (lvl + 1))
        args_s (indent lvl)
        (show_ret_ty ~lvl:(lvl + 1) ret)
  | RGeneric (name, args) ->
      show_qualified_name name ^ "<"
      ^ String.concat ", " (List.map (show_ty ~lvl) args)
      ^ ">"

and show_ret_ty ?(lvl = 0) = function
  | RetVoid -> "RetVoid"
  | RetVal t -> Printf.sprintf "RetVal(%s)" (show_ty ~lvl:(lvl + 1) t)

and show_ty ?(lvl = 0) = function
  | TParam name -> "TParam(" ^ name.elt ^ ")"
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
  | Array (elems, on_heap) ->
      let elems_s =
        String.concat ";\n"
          (List.map
             (fun e ->
               Printf.sprintf "%s%s" (indent (lvl + 2)) (show_node show_exp e))
             elems)
      in
      Printf.sprintf "%sArray([\n%s\n%s], on_heap=%b)" (indent lvl) elems_s
        (indent lvl) on_heap
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
        (show_ref_ty cn.elt) fields_s (indent lvl)
  | Lambda (scope, params, body) ->
      let scope_s =
        "[" ^ String.concat ", " (List.map (show_node show_exp) scope) ^ "]"
      in
      let params_s = String.concat ", " params in
      let body_s =
        String.concat ";\n" (List.map (show_node_stmt ~lvl:(lvl + 2)) body)
      in
      Printf.sprintf "%sLambda(scope=[%s], params=[%s], body=[\n%s\n%s])"
        (indent lvl) scope_s params_s body_s (indent lvl)
  | TypedLambda (scope, params, ret_ty, body) ->
      let scope_s =
        "[" ^ String.concat ", " (List.map (show_node show_exp) scope) ^ "]"
      in
      let params_s =
        String.concat "; "
          (List.map
             (fun (id, ty) -> Printf.sprintf "(%s : %s)" id (show_ty ty))
             params)
      in
      let body_s =
        String.concat ";\n" (List.map (show_node_stmt ~lvl:(lvl + 2)) body)
      in
      Printf.sprintf
        "%sTypedLambda(scope=[%s], params=[%s]; ret=%s; body=[\n%s\n%s])"
        (indent lvl) scope_s params_s
        (show_ret_ty ~lvl:(lvl + 1) ret_ty)
        body_s (indent lvl)
  | Conditional (cond, when_true, when_false) ->
      Printf.sprintf "%sConditional(\n%s%s,\n%s%s,\n%s%s)" (indent lvl)
        (indent (lvl + 1))
        (show_node show_exp cond)
        (indent (lvl + 1))
        (show_node show_exp when_true)
        (indent (lvl + 1))
        (show_node show_exp when_false)

(* Variable declarations *)

and show_vdecl ?(lvl = 0) (id, ty_opt, exp_opt, is_const) =
  Printf.sprintf "%sDecl{id=%s; ty=%s; const=%b;\n%sinit=%s}" (indent lvl) id
    (match ty_opt with Some ty -> show_ty ~lvl:(lvl + 1) ty | None -> "None")
    is_const
    (indent (lvl + 1))
    (if Option.is_some exp_opt then show_node show_exp (Option.get exp_opt)
     else "<default>")

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
  | Free es ->
      Printf.sprintf "%sFree %s" (indent lvl)
        (String.concat ", " (List.map (show_node show_exp) es))

and show_node_stmt ?(lvl = 0) stmt_node =
  Printf.sprintf "%s;\n" (show_node (show_stmt ~lvl) stmt_node)

(* Function declarations *)

let show_annotations annos =
  String.concat ",\n"
    (List.map
       (fun (i, e) ->
         "@"
         ^ show_node (fun i -> i) i
         ^ "\n"
         ^
         match e with
         | Some l -> String.concat "\n" (List.map (show_node show_exp) l)
         | None -> "")
       annos)
  ^ "\n"

let show_type_params params =
  match params with
  | [] -> ""
  | _ ->
      "<"
      ^ String.concat ", "
          (List.map
             (fun p ->
               p.pname.elt
               ^
               match p.bound with
               | None -> ""
               | Some b -> " impl " ^ show_ty b.elt)
             params)
      ^ ">"

let show_proto ?(lvl = 0)
    { elt = { annotations; frtyp; fname; args; type_params }; loc = _ } =
  let args_s =
    String.concat "; "
      (List.map
         (fun (ty, id) -> Printf.sprintf "(%s, %s)" (show_ty ty) id)
         args)
  in
  Printf.sprintf "%s[%s]proto{name=%s; ret=%s; args=[%s]}" (indent lvl)
    (show_annotations annotations)
    (fname ^ show_type_params type_params)
    (show_ret_ty ~lvl:(lvl + 1) frtyp)
    args_s

let show_fdecl ?(lvl = 0)
    { annotations; frtyp; fname; args; body; inline; type_params } =
  let args_s =
    String.concat "; "
      (List.map
         (fun (ty, id) -> Printf.sprintf "(%s, %s)" (show_ty ty) id)
         args)
  in
  Printf.sprintf
    "%s[%s]fdecl{name=%s; inline=%b; ret=%s; args=[%s]; body=[\n%s\n%s]}"
    (indent lvl)
    (show_annotations annotations)
    (fname ^ show_type_params type_params)
    inline
    (show_ret_ty ~lvl:(lvl + 1) frtyp)
    args_s
    (String.concat "" (List.map (show_node_stmt ~lvl:(lvl + 1)) body))
    (indent lvl)

(* Class and program printers *)

let show_field ?(lvl = 0) { fieldName; ftyp; init } =
  Printf.sprintf "%s%s: %s = %s" (indent lvl) fieldName
    (show_ty ~lvl:(lvl + 1) ftyp)
    (match init with Some e -> show_node show_exp e | None -> "None")

let show_cdecl ?(lvl = 0)
    { elt = { annotations; cname; impls; fields; methods; cparams }; loc = _ } =
  let fields_s =
    String.concat ";\n"
      (List.map (show_node (show_vdecl ~lvl:(lvl + 2))) fields)
  in
  let methods_s =
    String.concat "\n"
      (List.map (fun m -> show_fdecl ~lvl:(lvl + 2) m.elt) methods)
  in
  Printf.sprintf
    "[%s]%scdecl{name=%s; impls=[%s];\n\
     %sfields=[\n\
     %s\n\
     %s];\n\
     %smethods=[\n\
     %s\n\
     %s]}"
    (show_annotations annotations)
    (indent lvl)
    (cname ^ show_type_params cparams)
    (String.concat ", " (List.map (fun n -> show_ty n.elt) impls))
    (indent (lvl + 1))
    fields_s
    (indent (lvl + 1))
    (indent (lvl + 1))
    methods_s (indent lvl)

let show_decl ?(lvl = 0) d = show_fdecl ~lvl d.elt

let show_import { elt = { path; selection }; _ } =
  let path = show_qualified_name path in
  let body =
    match selection with
    | Module_import alias ->
        let alias =
          match alias with None -> "" | Some name -> " as " ^ name.elt
        in
        path ^ alias
    | Named_import names ->
        String.concat ", " (List.map (fun name -> name.elt) names)
        ^ " from " ^ path
  in
  "import " ^ body ^ ";"

let show_top_level { elt = { declaration; export_loc }; _ } =
  let prefix = match export_loc with None -> "" | Some _ -> "export " in
  let declaration =
    match declaration with
    | Function fn -> show_decl ~lvl:1 fn
    | Class cn -> show_cdecl ~lvl:1 cn
    | Prototype pn -> show_proto ~lvl:1 pn
    | Interface value ->
        "interface " ^ value.elt.iname
        ^ show_type_params value.elt.iparams
        ^ " { "
        ^ String.concat "; "
            (List.map
               (fun (f : ifield node) ->
                 (if f.elt.fconst then "const " else "let ")
                 ^ f.elt.fieldName ^ ": " ^ show_ty f.elt.ftyp)
               value.elt.fields
            @ List.map show_proto value.elt.protos)
        ^ " }"
    | Global { elt = { gname; gtyp; ginit; gconst }; _ } ->
        show_vdecl ~lvl:1 (gname, gtyp, ginit, gconst)
  in
  prefix ^ declaration

let show_prog (Prog (imports, declarations)) =
  let items =
    List.map show_import imports @ List.map show_top_level declarations
  in
  Printf.sprintf "Program{\n%s\n}" (String.concat "\n" items)
