open Ast
open Tctxt
open Conversions

exception TypeError of string

let make_error_underline c1 c2 =
  let spaces = String.make (c1 - 1) ' ' in
  let carets = String.make (c2 - c1) '^' in
  spaces ^ "\x1b[31m" ^ carets ^ "\x1b[0m"

let get_line filename lno =
  try
    let ic = open_in filename in
    let rec loop n =
      match input_line ic with
      | line when n = 1 -> line
      | _ -> loop (n - 1)
      | exception End_of_file -> Printf.sprintf "<line %d not found>" lno
    in
    let l = loop lno in 
    close_in ic; l
  with Sys_error _ ->
    Printf.sprintf "<source unavailable: %s>" filename
  
let type_error (l: 'a node) err =
  let (filename, (l1, c1), (_l2, c2)) = l.loc in
  let line = get_line filename l1 in
  let line_indicator = Printf.sprintf "%4d | " l1 in 
  let offset = String.length line_indicator in
  let underline = make_error_underline (c1+offset) (c2+offset) in
  raise (TypeError (
    Printf.sprintf
      "Error at %s:%d:%d:\n\
       %s%s\n\
            %s\n\
       %s"
      filename l1 c1
      line_indicator line underline
      err
  ))

let type_warning (l: 'a node) err = 
  let (_, (s, e), _) = l.loc in
  Printf.eprintf "[%d, %d] Warning: %s" s e err

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
  | RRange (t1, t2) -> typecheck_ty l tc t1 ; typecheck_ty l tc t2
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
      | None -> let new_tc = Tctxt.add_global tc fn.elt.fname (convert_ty func_type) in aux new_tc t
    )
  | [] -> tc
  in aux tc fns 

let signed_int_hierarchy: Typed_ast.sint list = [Ti8; Ti16; Ti32; Ti64; Ti128]
let unsigned_int_hierarchy: Typed_ast.uint list = [Tu8; Tu16; Tu32; Tu64; Tu128]
let float_hierarchy: Typed_ast.float_ty list = [Tf32; Tf64]

let widest_int (ity1: Typed_ast.int_ty) (ity2: Typed_ast.int_ty) (n: 'a node) : Typed_ast.int_ty =
  let find_index l e = 
    let rec aux i = function 
    | [] ->  type_error n "Invalid int type spec"
    | h::t -> if h = e then i else aux (i+1) t 
  in aux 0 l 
  in
  let max a b = if a > b then a else b in 
  let widest l t1 t2 = 
    (List.nth l 
    (max (find_index l t1) (find_index l t2))) in
  let widest' t1 t2 =
    (List.nth signed_int_hierarchy
    (max (find_index signed_int_hierarchy t1) (find_index unsigned_int_hierarchy t2))) in
  match ity1, ity2 with 
  | TSigned s, TUnsigned u
  | TUnsigned u, TSigned s -> 
    TSigned (widest' s u)

  | TSigned t1, TSigned t2 -> 
    TSigned (widest signed_int_hierarchy t1 t2)

  | TUnsigned t1, TUnsigned t2 -> 
    TUnsigned (widest unsigned_int_hierarchy t1 t2)

let widest_float (fty1: Typed_ast.float_ty) (fty2: Typed_ast.float_ty) : Typed_ast.float_ty  =
  if fty1 = fty2 then fty1 else Tf64

let meet_number (n: 'a node) : Typed_ast.ty * Typed_ast.ty -> Typed_ast.ty = function
  | TInt i1, TInt i2 -> TInt (widest_int i1 i2 n)
  | TFloat f, TInt i 
  | TInt i, TFloat f when i <> TSigned Ti64 && i <> TUnsigned Tu64 -> TFloat f
  | TFloat _, TInt _
  | TInt _, TFloat _ -> TFloat Tf64
  | TFloat f1, TFloat f2 -> TFloat (widest_float f1 f2)
  | _ -> type_error n "unreachable state: meeting non-numbers."

let is_number (t: Typed_ast.ty) : bool = 
  match t with 
  | Typed_ast.TInt _ | Typed_ast.TFloat _ -> true 
  | _ -> false 

let all_numbers (tl: Typed_ast.ty list) : bool = 
  List.for_all is_number tl

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
    | Some t -> (Id i, t)
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

  | Bop (binop, e1, e2) -> 
    let te1, lty = type_exp tc e1 in 
    let te2, rty = type_exp tc e2 in 
    let binop' = convert_binop binop in 
    let res_ty = (match binop with 
    | Eqeq | Neq | Gt | Gte | Lt | Lte -> 
      if (subtype tc lty rty) && (subtype tc rty lty) then Typed_ast.TBool
      else type_error e "== or != used with non type-compatible arguments"
    | And | Or -> 
      if lty = Typed_ast.TBool && rty = Typed_ast.TBool then Typed_ast.TBool 
        else type_error e "&& or || used on non-bool arguments"
    | At -> type_error e "@ not yet supported."
    | _ -> 
      let args_valid = subtype tc lty rty || all_numbers [lty ; rty] in
      if not args_valid then 
        type_error e "using binary operator on non-number types"
      else 
        meet_number e (lty, rty)
    ) in Typed_ast.Bop(binop', te1, te2, res_ty), res_ty

  | Uop (unop, e1) -> 
    let te1, ety = type_exp tc e1 in 
    let unop' = convert_unop unop in 
    let res_ty = (match unop, ety with 
      | Neg, t when is_number t -> t
      | Not, t when t  = TBool -> TBool
      | _ -> type_error e "bad type for neg or not operator")
    in Typed_ast.Uop(unop', te1, res_ty), res_ty

  | Index (e_iter, e_idx) -> 
    let t_iter, iter_ty = type_exp tc e_iter in 
    let arr_ty = (match iter_ty with 
      | Typed_ast.TRef (Typed_ast.RArray arr_ty') -> arr_ty'
      | _ -> type_error e "cannot index non-array type") in
    let t_idx, idx_ty = type_exp tc e_idx in 
    let _ = (match idx_ty with 
      | Typed_ast.TInt _ -> ()
      | _ -> type_error e "index must be integer type") in 
    Typed_ast.Index(t_iter, t_idx, arr_ty), arr_ty
  
  | Range (el, er, incl) -> 
    let tel, el_ty = type_exp tc el in 
    let ter, er_ty = type_exp tc er in 
    if all_numbers [el_ty ; er_ty] then 
      (* TODO figure out if we should check if bounds are mixed floats/ints or not *)
      Typed_ast.Range (tel, ter, incl), Typed_ast.TRef (Typed_ast.RRange (el_ty, er_ty))
    else  
      type_error e "Range must have numeric bounds..."

  | _ -> type_error e "not supported yet"

let rec type_block (tc : Tctxt.t) (frtyp : Ast.ret_ty) (stmts : Ast.stmt node list) (in_loop: bool) : Tctxt.t * Typed_ast.stmt list =
  let tc_new, rev_stmts =
    List.fold_left 
      (fun (tc_acc, tstmts) s -> 
        let (tc', tstmt) = type_stmt tc_acc frtyp s in_loop in 
        (tc', tstmt::tstmts))
        (tc, [])
        stmts 
  in tc_new, List.rev rev_stmts
      
and type_stmt (tc: Tctxt.t) (frtyp: Ast.ret_ty) (stmt_n: Ast.stmt node) (in_loop: bool) : (Tctxt.t * Typed_ast.stmt) = 
  let {elt=stmt; loc=_} = stmt_n in
  match stmt with 
  | Ast.Decl (i, ty_opt, en, const) -> 
    let te, e_ty = type_exp tc en in 
    let tc', resolved_ty = (match ty_opt with 
      | None -> add_local tc i e_ty, e_ty
      | Some given_ty -> 
        let resolved = 
          if convert_ty given_ty <> e_ty 
            then type_error stmt_n ("Provided type " ^ (Ast.show_ty given_ty) ^ " does not match inferred type " ^ (Typed_ast.show_ty e_ty))
          else e_ty 
        in
        add_local tc i resolved, resolved)
    in tc', Typed_ast.Decl(i, resolved_ty, te, const)

  | Ast.Assn (e1, op, e2) -> 
    let te1, _e1ty = type_exp tc e1 in 
    let te2, _e2ty = type_exp tc e2 in 
    tc, Typed_ast.Assn(te1, convert_aop op, te2)

  | Ast.Ret expr -> 
    let te_opt = (match expr with 
    | Some e -> 
      let te, _expr_ty = type_exp tc e in 
      Some(te)
    | None -> None) in tc, Typed_ast.Ret te_opt

  | Ast.SCall (en, ens) -> 
    let te, fty = type_exp tc en in
    begin
      match fty with 
      | Typed_ast.TRef (Typed_ast.RFun (_, Typed_ast.RetVoid)) -> ()
      | Typed_ast.TRef (Typed_ast.RFun (_, _)) -> type_warning stmt_n "Ignoring non-void function"
      | _ -> type_error stmt_n "How did we manage to parse this as a function call?"
    end;
    let t_ens = List.map (fun en' -> type_exp tc en' |> fst) ens in 
    tc, Typed_ast.SCall (te, t_ens)

  | Ast.If (cond, then_branch, else_branch) -> 
    let tcond, cond_ty = type_exp tc cond in
    if cond_ty <> Typed_ast.TBool then
      type_error cond "if condition must be bool";
    let (_tc_then, t_then) = type_block tc frtyp then_branch in_loop in
    let (_tc_else, t_else) = type_block tc frtyp else_branch in_loop in
    tc, Typed_ast.If(tcond, t_then, t_else)

  | Ast.While (cond, body) -> 
    let tcond, cond_ty = type_exp tc cond in 
    if cond_ty <> Typed_ast.TBool then 
      type_error cond "while condition must be bool";
    let (_tc_while, t_body) = type_block tc frtyp body true in 
    tc, Typed_ast.While(tcond, t_body)

  | Ast.For (i_node, iter_exp, step_opt, body) -> 
    let titer, iter_ty = type_exp tc iter_exp in
    let elem_ty =
      match iter_ty with
      | Typed_ast.TRef (Typed_ast.RArray t) -> t
      | Typed_ast.TRef (Typed_ast.RString) -> Typed_ast.TInt (Typed_ast.TSigned Typed_ast.Ti8)   (* chars in string *)
      | Typed_ast.TRef (Typed_ast.RRange (t1, _t2)) -> t1
      | _ ->
          type_error iter_exp "for loop must iterate over an array, string, or range"
    in
    let t_step =
      match (iter_ty, step_opt) with
      | (Typed_ast.TRef (Typed_ast.RRange _), Some step_exp) ->
          let ts, step_ty = type_exp tc step_exp in
          begin 
          if (is_number step_ty) then 
            ts
          else 
            type_error step_exp "for loop step must be an integer"
          end
      | (Typed_ast.TRef (Typed_ast.RRange _), None) -> Typed_ast.Int (1L, Typed_ast.TSigned Typed_ast.Ti32)
      | (_, Some _) ->
          type_error iter_exp "step is only allowed when iterating over ranges"
      | (_, None) -> 
          (* no step needed for strings/arrays *)
          Typed_ast.Int (1L, Typed_ast.TSigned Typed_ast.Ti32)  (* ignored *)
    in
    let tc_loop = add_local tc i_node.elt elem_ty in
    let (_tc_body, t_body) = type_block tc_loop frtyp body true in
    (tc, Typed_ast.For (i_node.elt, titer, t_step, t_body))

  | Ast.Break -> 
    if (not in_loop) then type_error stmt_n "break can only be used inside loop" else tc, Typed_ast.Break

  | Ast.Continue -> 
    if (not in_loop) then type_error stmt_n "continue can only be used inside loop" else tc, Typed_ast.Continue

let type_fn (tc: Tctxt.t) (fn: fdecl node) : (Typed_ast.fdecl) = 
  let { elt = f; loc = _ } = fn in
  let { frtyp; fname; args; body } = f in
  let tc' = List.fold_left 
    (fun acc (t,a) -> 
      typecheck_ty fn acc t; 
      add_local acc a (convert_ty t)
    ) tc args in 
  let frtyp' = convert_ret_ty frtyp in
  let args' = List.map (fun (ty, id) -> (convert_ty ty, id)) args in
  let (_tc_final, typed_body) = type_block tc' frtyp body false in 
  {
    frtyp=frtyp';
    fname;
    args=args';
    body = typed_body;
  }

let type_program (prog: Ast.program) : Typed_ast.program =
  (* create global var ctxt *)
  (* create class ctxt *)
  let fc = create_fn_ctxt (Tctxt.empty) prog in 
  let (Prog fns) = prog in
  let fns_t = List.map (fun fn -> type_fn fc fn) fns in 
  Prog fns_t


let type_prog (prog: Ast.program) : (Typed_ast.program, Core.Error.t) result= 
  try Ok (type_program prog) with 
  | TypeError msg -> 
    let err = Fmt.str "Type Error: %s" msg in
    Error (Core.Error.of_string err)