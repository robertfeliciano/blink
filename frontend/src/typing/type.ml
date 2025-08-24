open Ast
open Tctxt


exception TypeError of string
let type_error (l: 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

let convert_sint (s : Ast.sint) : Typed_ast.sint =
  match s with
  | Ast.Ti8 -> Typed_ast.Ti8
  | Ast.Ti16 -> Typed_ast.Ti16
  | Ast.Ti32 -> Typed_ast.Ti32
  | Ast.Ti64 -> Typed_ast.Ti64
  | Ast.Ti128 -> Typed_ast.Ti128

let convert_uint (u : Ast.uint) : Typed_ast.uint =
  match u with
  | Ast.Tu8 -> Typed_ast.Tu8
  | Ast.Tu16 -> Typed_ast.Tu16
  | Ast.Tu32 -> Typed_ast.Tu32
  | Ast.Tu64 -> Typed_ast.Tu64
  | Ast.Tu128 -> Typed_ast.Tu128

let convert_float_ty (f : Ast.float_ty) : Typed_ast.float_ty =
  match f with
  | Ast.Tf32 -> Typed_ast.Tf32
  | Ast.Tf64 -> Typed_ast.Tf64

let rec convert_int_ty (i : Ast.int_ty) : Typed_ast.int_ty =
  match i with
  | Ast.TSigned s -> Typed_ast.TSigned (convert_sint s)
  | Ast.TUnsigned u -> Typed_ast.TUnsigned (convert_uint u)
  
let rec convert_ret_ty (rt : Ast.ret_ty) : Typed_ast.ret_ty =
  match rt with
  | Ast.RetVoid -> Typed_ast.RetVoid
  | Ast.RetVal t -> Typed_ast.RetVal (convert_ty t)

and convert_ty (t : Ast.ty) : Typed_ast.ty =
  match t with
  | Ast.TBool -> Typed_ast.TBool
  | Ast.TInt it -> Typed_ast.TInt (convert_int_ty it)
  | Ast.TFloat ft -> Typed_ast.TFloat (convert_float_ty ft)
  | Ast.TRef r -> Typed_ast.TRef (convert_ref_ty r)

and convert_ref_ty (r : Ast.ref_ty) : Typed_ast.ref_ty =
  match r with
  | Ast.RString -> Typed_ast.RString
  | Ast.RArray t -> Typed_ast.RArray (convert_ty t)
  | Ast.RFun (tl, rt) ->
      Typed_ast.RFun (List.map convert_ty tl, convert_ret_ty rt)

let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with
  | TInt _
  | TFloat _
  | TBool -> ()
  | TRef r -> typecheck_rty l tc r 
and typecheck_rty  (l : 'a Ast.node) (tc : Tctxt.t) (r : Ast.ref_ty) : unit =
  match r with
  | RString -> ()
  | RArray t -> typecheck_ty l tc t 
  | RFun (tl, rt) -> (List.iter (typecheck_ty l tc) tl; typecheck_ret_ty l tc rt)
and typecheck_ret_ty (l : 'a Ast.node) (tc : Tctxt.t) (rt : Ast.ret_ty) : unit =
  match rt with
  | RetVoid -> ()
  | RetVal t -> typecheck_ty l tc t

let get_fdecl_type (fn: fdecl node) (tc: Tctxt.t) : Ast.ty = 
  let {elt={frtyp; fname = _; args; body = _}; loc = _} = fn in 
  let arg_types = List.map (fun (t,_) -> (typecheck_ty fn tc t; t)) args in
  (
    typecheck_ret_ty fn tc frtyp;
    List.iter (typecheck_ty fn tc) arg_types;
    TRef (RFun (arg_types, frtyp))
  )

let create_fn_ctxt (tc: Tctxt.t) (Prog fns: Ast.program) : Tctxt.t = 
  let rec aux (tc: Tctxt.t) = function
  | fn::t -> 
    let func_type = get_fdecl_type fn tc in (
      match lookup_global_option fn.elt.fname tc with
      | Some _ -> type_error fn (Printf.sprintf "function with name %s already exists" fn.elt.fname)
      | None -> let new_tc = Tctxt.add_global tc fn.elt.fname func_type in aux new_tc t
    )
  | [] -> tc
  in aux tc fns 

let rec subtype (tc : Tctxt.t) (t1 : Typed_ast.ty) (t2 : Typed_ast.ty) : bool =
  match t1, t2 with 
  | TBool, TBool
  | TInt _, TInt _
  | TFloat _, TFloat _ -> true
  | TRef t1', TRef t2' -> subtype_ref tc t1' t2'
  | _ -> false

and subtype_ref (tc : Tctxt.t) (t1 : Typed_ast.ref_ty) (t2 : Typed_ast.ref_ty) : bool = 
  match t1, t2 with 
  | RString, RString -> true
  | RArray t1', RArray t2' -> subtype tc t1' t2'
  (* | RClass c1, RClass c2 -> subtype_class tc c1 c2 *)
  | RFun(pty1, rty1), RFun(pty2, rty2) -> let subtype_arg = fun (ty1) (ty2) -> subtype tc ty2 ty1 in
  (List.for_all2 subtype_arg pty1 pty2) && (subtype_ret_ty tc rty1 rty2)
  | _ -> false

and subtype_list tc l1 l2 : bool = 
  if List.length l1 != List.length l2 then false 
  else List.fold_left2 (fun a x y -> a && subtype tc x y ) true l1 l2

and subtype_ret_ty (tc: Tctxt.t) (t1: Typed_ast.ret_ty) (t2: Typed_ast.ret_ty) : bool =
  match t1,t2 with
  | RetVoid, RetVoid -> true
  | RetVal t1', RetVal t2' -> subtype tc t1' t2'
  | _ -> false

let rec type_exp (tc: Tctxt.t) (e: Ast.exp node) : (Typed_ast.exp * Typed_ast.ty) = 
  let {elt=e';loc=_} = e in 
  match e' with
  | Bool b -> (Typed_ast.Bool b, Typed_ast.TBool)
  | Int i -> let ty = Typed_ast.TInt (Typed_ast.TSigned Typed_ast.Ti32) in
    (Typed_ast.Int (i, Typed_ast.TSigned Typed_ast.Ti32), ty)
  | Float f -> let ty = Typed_ast.TFloat (Typed_ast.Tf64) in 
    (Typed_ast.Float (f, (Typed_ast.Tf64)), ty)
  | Str s -> let ty = Typed_ast.TRef (Typed_ast.RString) in 
    (Typed_ast.Str s, ty)
  | Id i -> (match Tctxt.lookup_option i tc with 
    | Some t -> (Id i, convert_ty t)
    | None -> type_error e ("variable " ^ i ^ " is not defined"))
  | Call (f, args) -> 
    (match snd @@ type_exp tc f with 
    | TRef (RFun (arg_types, RetVal rt)) -> 
      (try (
        let typed_callee, _ = type_exp tc f in 
        let typed_args = List.map2 
            (fun aty a -> 
              let te, ty = type_exp tc a in 
              if (subtype tc ty aty) then te else type_error e ("invalid argument type for " ^ show_exp a.elt)
            ) arg_types args in 
        (Typed_ast.Call (typed_callee,  typed_args, rt), rt)
      ) with Invalid_argument _ -> type_error e "invalid number of arguments supplied")
    | TRef (RFun (_, RetVoid)) -> type_error e "assigning void function return to variable."
    | _ -> type_error e "attempted to call a non-function type." )
  | x -> ()
  
let type_stmt (tc: Tctxt.t) (frtyp: Ast.ret_ty) (stmt: Ast.stmt) : (Tctxt.t * Typed_ast.stmt) = 
  match stmt with 
  | Ast.Decl v -> 
      (* Add logic to typecheck standalone expressions *)
      ()
  | Ast.Assn (e1, op, e2) -> 
      (* Add logic to typecheck the assignment *)
      ()
  | Ast.Ret expr -> 
      (* Add logic to typecheck the return statement *)
      ()
  | Ast.SCall (en, ens) -> 
      ()
  | Ast.If (cond, then_branch, else_branch) -> 
      (* Add logic to typecheck if-else statements *)
      ()
  | Ast.While (cond, body) -> 
      (* Add logic to typecheck while loops *)
      ()
  | Ast.For (i, e1, e2, body) -> 
      ()
  | Ast.Break -> Typed_ast.Break
  | Ast.Continue -> Typed_ast.Continue

let type_fn (tc: Tctxt.t) (fn: fdecl node) : (Typed_ast.fdecl) = 
  let { elt = f; loc = _ } = fn in
  let { frtyp; fname; args; body } = f in
  let tc' = List.fold_left 
  (fun acc (t,a) -> 
    typecheck_ty fn acc t; 
    add_local acc a t
  ) tc args in 
  let frtyp' = convert_ret_ty frtyp in
  let args' = List.map (fun (ty, id) -> (convert_ty ty, id)) args in
  let typed_body = List.map (type_stmt tc' frtyp) body in 
  {
    frtyp=frtyp';
    fname;
    args=args';
    body = typed_body;
  }

let type_program (prog: Ast.program) : unit =
  (* create global var ctxt *)
  (* create class ctxt *)
  let fc = create_fn_ctxt (Tctxt.empty) prog in 
  let (Prog fns) = prog in
  let fns_t = List.map (fun fn -> type_fn fc fn) fns in ()