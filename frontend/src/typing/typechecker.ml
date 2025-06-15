open Ast
open Tctxt

exception TypeError of string

let type_error (l: 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

let typecheck_prog = function
| Gfdecl f -> Gfdecl f
(* | other -> other *)

let signed_int_hierarchy: sint list = [Ti8; Ti16; Ti32; Ti64; Ti128]
let unsigned_int_hierarchy: uint list = [Tu8; Tu16; Tu32; Tu64; Tu128]
let float_hierarchy: float_ty list = [Tf32; Tf64]


let widest_int (ity1: int_ty) (ity2: int_ty) (n: 'a node) : int_ty =
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

let widest_float (fty1: float_ty) (fty2: float_ty) : float_ty  =
  if fty1 = fty2 then fty1 else Tf64

let meet_number (n: 'a node) : ty * ty -> ty = function
  | TInt i1, TInt i2 -> TInt (widest_int i1 i2 n)
  | TFloat f, TInt i 
  | TInt i, TFloat f when i <> TSigned Ti64 && i <> TUnsigned Tu64 -> TFloat f
  | TFloat _, TInt _
  | TInt _, TFloat _ -> TFloat Tf64
  | TFloat f1, TFloat f2 -> TFloat (widest_float f1 f2)
  | _ -> type_error n "unreachable state: meeting non-numbers."

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
  (* | RClass i -> (
    match Tctxt.lookup_class_option i tc with
    | Some _ -> ()
    | None -> type_error l ("stuct " ^ i ^ " is not defined")) *)
  | RFun (tl, rt) -> (List.iter (typecheck_ty l tc) tl; typecheck_ret_ty l tc rt)

and typecheck_ret_ty (l : 'a Ast.node) (tc : Tctxt.t) (rt : Ast.ret_ty) : unit =
  match rt with
  | RetVoid -> ()
  | RetVal t -> typecheck_ty l tc t

let rec subtype (tc : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool =
  match t1, t2 with 
  | TBool, TBool
  | TInt _, TInt _
  | TFloat _, TFloat _ -> true
  | TRef t1', TRef t2' -> subtype_ref tc t1' t2'
  | _ -> false

and subtype_ref (tc : Tctxt.t) (t1 : Ast.ref_ty) (t2 : Ast.ref_ty) : bool = 
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

(* and subtype_class (tc: Tctxt.t) (s1: id) (s2: id) : bool = 
  s1 = s2 || subtype_fields tc s1 s2 *)

(* and subtype_fields c n1 n2 : bool =
  let fields1 = Tctxt.lookup_class n1 c in
  let fields2 = Tctxt.lookup_class n2 c in
  let rec helper l1 l2 =
    match (l1, l2) with
    | _, [] -> true
    | [], _ -> false
    | f1::t1, f2::t2 -> f1.fieldName = f2.fieldName && f1.ftyp = f2.ftyp
                                                    && helper t1 t2 in
  helper fields1 fields2 *)


and subtype_ret_ty (tc: Tctxt.t) (t1: Ast.ret_ty) (t2: Ast.ret_ty) : bool =
  match t1,t2 with
  | RetVoid, RetVoid -> true
  | RetVal t1', RetVal t2' -> subtype tc t1' t2'
  | _ -> false

let all_numbers (tl: ty list) : bool = 
  List.for_all (
    fun t ->  
      match t with 
      | TInt _ | TFloat _ -> true 
      | _ -> false ) 
    tl

let type_of_binop (n: exp node) (b:binop) (l:ty) (r:ty) : ty * ty * ty = 
  let res_ty = match l,r with 
  | TInt _, TInt _
  | TFloat _, TFloat _
  | TInt _, TFloat _
  | TFloat _, TInt _ -> meet_number n (l,r)
  | _ when l = r -> l (* bools, strings, arrays result in themselves by default *)
  | _ -> type_error n "unable to reconcile types"
  in match b with 
  | Add | Sub | Mul | Div | Mod | Pow | At -> l, r, res_ty
  | Lt | Lte | Gt | Gte | Eqeq | Neq -> l, r, TBool
  | And | Or -> TBool, TBool, TBool

let rec typecheck_exp (tc: Tctxt.t) (e: Ast.exp node) : Ast.ty =
  let {elt; loc=_} = e in
  match elt with 
  | Bool _ -> TBool
  | Int _ -> TInt (TSigned Ti32)
  | Float _ -> TFloat Tf64
  | Str _ -> TRef RString
  | Id i -> (
    match Tctxt.lookup_option i tc with 
    |Some t -> t
    |None -> type_error e ("variable " ^ i ^ " is not defined")
  )
  | Call (f, args) -> 
    (match typecheck_exp tc f with 
    | TRef (RFun (arg_types, RetVal rt)) -> 
      (try (
        List.iter2 (
          fun aty a -> if (subtype tc (typecheck_exp tc a) aty) then () else type_error e "invalid argument type"
        )
        arg_types args;
        rt
      ) with Invalid_argument _ -> type_error e "invalid number of arguments supplied")
    | TRef (RFun (_, RetVoid)) -> type_error e "assigning void function return to variable."
    | _ -> type_error e "attempted to call a non-function type." )
  | Array ens -> 
    let t = List.hd ens |> typecheck_exp tc in 
    let ts = List.map (typecheck_exp tc) ens in
    if List.for_all (fun t' -> subtype tc t t') ts then TRef (RArray t) 
    else type_error e "mismatched type in array"
  | Bop (b, l, r) -> 
    let lt = typecheck_exp tc l in 
    let rt = typecheck_exp tc r in 
    let final = match b with 
      | Eqeq | Neq -> 
        if (subtype tc lt rt) && (subtype tc rt lt) then TBool 
        else type_error e "== or != used with non type-compatible arguments"
      | And | Or -> 
        if lt = TBool && rt = TBool then TBool
        else type_error e "&& or || used on non-bool arguments"
      | At -> TBool
        (* (match lt, rt with 
        | TRef (RArray lt'), TRef (RArray rt') -> 
          if all_numbers [lt' ; rt'] then meet_number e (lt', rt')
          else type_error e "@ called on non-numeric array"
        | TRef (RClass lt'), TRef (RClass rt') when lt' = "Matrix" && rt' = "Matrix" -> 
          let fields = lookup_class lt' tc in 
          (* lookup_field_option *) TBool
          (* else type_error e (lt' ^ " must implement __dot__") *)
        | _ -> type_error e "@ called on non-array/matrix args") *)
      | _ -> 
        let args_valid = subtype tc lt rt || all_numbers [lt ; rt] in
        if not args_valid then 
          type_error e "using binary operator on non-number types"
        else 
          meet_number e (lt, rt)
        in final
  | Uop (u, operand) -> 
    let ot = typecheck_exp tc operand in 
    (match u, ot with 
    | Neg, t when all_numbers [t] -> t
    | Not, t when t  = TBool -> TBool
    | _ -> type_error e "bad type for neg or not operator")
    
  | _ -> TBool
  
let get_fdecl_type (e: Ast.fdecl node) tc = 
  let {elt={frtyp; fname=_; args; body=_}; loc=_} = e in
  let arg_types = List.map (fun (t, _) -> typecheck_ty e tc t ; t) args in
  typecheck_ret_ty e tc frtyp; 
  List.iter (typecheck_ty e tc) arg_types;
  TRef (RFun (arg_types, frtyp))

let rec create_function_ctxt (p: Ast.prog) (tc: Tctxt.t) : Tctxt.t =
  match p with 
  | Gfdecl ({elt={frtyp; fname; args; body}; loc})::t ->
    let this_node = {elt={frtyp; fname; args; body}; loc} in
    (match lookup_global_option fname tc with 
    | Some _ -> type_error this_node (Printf.sprintf "function with name %s already exists" fname)
    | None -> 
      let func_type = get_fdecl_type this_node tc in
      Tctxt.add_global tc fname func_type |> create_function_ctxt t)
  (* | _::t -> create_function_ctxt t tc *)
  | [] -> tc

(* let rec typecheck_exp 

let typecheck_stmt (tc: Tctxt.t) (s: stmt node) (rty: ret_ty) : Tctxt.t = 
  let {elt=stmt; loc=stmt_loc} = s in
  match stmt with 
  | Assn ({elt=lhs; loc=lhs_loc}, aop, {elt=rhs; loc=rhs_loc}) -> 
    match lhs with 
    | _ -> 

    Assn ({elt=lhs; loc=lhs_loc}, aop=aop, e=e)
  | _ -> _ *)


let typecheck ast = 
  List.map typecheck_prog ast