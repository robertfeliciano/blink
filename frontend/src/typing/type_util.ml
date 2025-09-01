open Ast
module Printer = Pprint_typed_ast

exception TypeError of string

let make_error_underline c1 c2 =
  let spaces = String.make (c1 - 1) ' ' in
  let carets = String.make (c2 - c1) '^' in
  spaces ^ "\x1b[1;31m" ^ carets ^ "\x1b[0;0m"

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
    close_in ic;
    l
  with Sys_error _ -> Printf.sprintf "<source unavailable: %s>" filename

let type_error (l : 'a node) err =
  let filename, (l1, c1), (_l2, c2) = l.loc in
  let line = get_line filename l1 in
  let line_indicator = Printf.sprintf "%4d | " l1 in
  let offset = String.length line_indicator in
  let underline = make_error_underline (c1 + offset) (c2 + offset) in
  raise
    (TypeError
       (Printf.sprintf "Error at %s:%d:%d:\n%s%s\n%s\n%s" filename l1 c1
          line_indicator line underline err))

let type_warning (l : 'a node) err =
  let _, (s, e), _ = l.loc in
  Printf.eprintf "[%d, %d] Warning: %s" s e err

let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with
  | TInt _ | TFloat _ | TBool -> ()
  | TRef r -> typecheck_rty l tc r

and typecheck_rty (l : 'a Ast.node) (tc : Tctxt.t) (r : Ast.ref_ty) : unit =
  match r with
  | RString -> ()
  | RArray (t, sz) ->
      if Z.lt sz (Z.of_int 0) then type_error l "negative length specified"
      else typecheck_ty l tc t
  | RRange (t1, t2) ->
      typecheck_ty l tc t1;
      typecheck_ty l tc t2
  | RFun (tl, rt) ->
      List.iter (typecheck_ty l tc) tl;
      typecheck_ret_ty l tc rt

and typecheck_ret_ty (l : 'a Ast.node) (tc : Tctxt.t) (rt : Ast.ret_ty) : unit =
  match rt with RetVoid -> () | RetVal t -> typecheck_ty l tc t

let get_fdecl_type (fn : fdecl node) (tc : Tctxt.t) : Ast.ty =
  let { elt = { frtyp; fname = _; args; body = _ }; loc = _ } = fn in
  let arg_types =
    List.map
      (fun (t, _) ->
        typecheck_ty fn tc t;
        t)
      args
  in
  typecheck_ret_ty fn tc frtyp;
  List.iter (typecheck_ty fn tc) arg_types;
  TRef (RFun (arg_types, frtyp))

let signed_int_hierarchy : Typed_ast.sint list =
  [ Ti8; Ti16; Ti32; Ti64; Ti128 ]

let unsigned_int_hierarchy : Typed_ast.uint list =
  [ Tu8; Tu16; Tu32; Tu64; Tu128 ]

let float_hierarchy : Typed_ast.float_ty list = [ Tf32; Tf64 ]

let widest_int (ity1 : Typed_ast.int_ty) (ity2 : Typed_ast.int_ty) (n : 'a node)
    : Typed_ast.int_ty =
  let find_index l e =
    let rec aux i = function
      | [] -> type_error n "Invalid int type spec"
      | h :: t -> if h = e then i else aux (i + 1) t
    in
    aux 0 l
  in
  let max a b = if a > b then a else b in
  let widest l t1 t2 = List.nth l (max (find_index l t1) (find_index l t2)) in
  let widest' t1 t2 =
    List.nth signed_int_hierarchy
      (max
         (find_index signed_int_hierarchy t1)
         (find_index unsigned_int_hierarchy t2))
  in
  match (ity1, ity2) with
  | TSigned Ti64, TUnsigned Tu64 | TUnsigned Tu64, TSigned Ti64 ->
      type_error n "cannot implicitly combine u64 and i64"
  | TSigned s, TUnsigned u | TUnsigned u, TSigned s -> TSigned (widest' s u)
  | TSigned t1, TSigned t2 -> TSigned (widest signed_int_hierarchy t1 t2)
  | TUnsigned t1, TUnsigned t2 ->
      TUnsigned (widest unsigned_int_hierarchy t1 t2)

let widest_float (fty1 : Typed_ast.float_ty) (fty2 : Typed_ast.float_ty) :
    Typed_ast.float_ty =
  if fty1 = fty2 then fty1 else Tf64

let meet_number (n : 'a node) : Typed_ast.ty * Typed_ast.ty -> Typed_ast.ty =
  function
  | TInt i1, TInt i2 -> TInt (widest_int i1 i2 n)
  | (TFloat f, TInt i | TInt i, TFloat f)
    when i <> TSigned Ti64 && i <> TUnsigned Tu64 ->
      TFloat f
  | TFloat _, TInt _ | TInt _, TFloat _ -> TFloat Tf64
  | TFloat f1, TFloat f2 -> TFloat (widest_float f1 f2)
  | _ -> type_error n "unreachable state: meeting non-numbers."

let is_number (t : Typed_ast.ty) : bool =
  match t with Typed_ast.TInt _ | Typed_ast.TFloat _ -> true | _ -> false

let all_numbers (tl : Typed_ast.ty list) : bool = List.for_all is_number tl

let rec equal_ty (t1 : Typed_ast.ty) (t2 : Typed_ast.ty) : bool =
  match (t1, t2) with
  | TBool, TBool -> true
  | TInt k1, TInt k2 -> k1 = k2
  | TFloat f1, TFloat f2 -> f1 = f2
  | TRef r1, TRef r2 -> equal_ref_ty r1 r2
  | _ -> false

and equal_ref_ty (r1 : Typed_ast.ref_ty) (r2 : Typed_ast.ref_ty) : bool =
  match (r1, r2) with
  | RString, RString -> true
  | RArray (t1, sz1), RArray (t2, sz2) -> sz1 = sz2 && equal_ty t1 t2
  | RFun (params1, RetVal rt1), RFun (params2, RetVal rt2) ->
      List.length params1 = List.length params2
      && List.for_all2 equal_ty params1 params2
      && equal_ty rt1 rt2
  | RFun (_params1, RetVoid), RFun (_params2, RetVoid) ->
      (* parameter lists must be *equal* to be equal types *)
      (* if you want exact function type equality incl. params: *)
      false
  | _ -> false

and equal_ret_ty (r1 : Typed_ast.ret_ty) (r2 : Typed_ast.ret_ty) : bool =
  match (r1, r2) with
  | RetVoid, RetVoid -> true
  | RetVal rv1, RetVal rv2 -> equal_ty rv1 rv2
  | _ -> false

let rec subtype (tc : Tctxt.t) (t1 : Typed_ast.ty) (t2 : Typed_ast.ty) : bool =
  match (t1, t2) with
  | TBool, TBool
  | TInt _, TInt _ 
  | TFloat _, TFloat _ 
  | TInt _, TFloat _ 
  | TFloat _, TInt _ -> true
  | TRef t1', TRef t2' -> subtype_ref tc t1' t2'
  | _ -> false

and subtype_ref (tc : Tctxt.t) (t1 : Typed_ast.ref_ty) (t2 : Typed_ast.ref_ty) :
    bool =
  match (t1, t2) with
  | RString, RString -> true
  | RArray (t1', sz1), RArray (t2', sz2) -> sz1 = sz2 && subtype tc t1' t2'
  | RFun (pty1, rty1), RFun (pty2, rty2) ->
      let contrav_params =
        List.length pty1 = List.length pty2
        && List.for_all2 (fun a1 a2 -> equal_ty a2 a1) pty1 pty2
      in
      contrav_params && subtype_ret_ty tc rty1 rty2
  | _ -> false

and subtype_ret_ty (tc : Tctxt.t) (t1 : Typed_ast.ret_ty)
    (t2 : Typed_ast.ret_ty) : bool =
  match (t1, t2) with
  | RetVoid, RetVoid -> true
  | RetVal t1', RetVal t2' -> subtype tc t1' t2'
  | _ -> false

let infer_integer_ty (n : Z.t) (e : exp node) : Typed_ast.int_ty =
  let open Z in
  if geq n (of_int32 Int32.min_int) && leq n (of_int32 Int32.max_int) then
    TSigned Ti32
  else if geq n (of_int64 Int64.min_int) && leq n (of_int64 Int64.max_int) then
    TSigned Ti64
  else if geq n zero && leq n (Z.of_string "18446744073709551615") then
    (* max uint64*)
    TUnsigned Tu64
  else type_error e ("integer literal `" ^ Z.to_string n ^ "` too large")

let fits_in_int_ty (n : Z.t) (t : Typed_ast.int_ty) : bool =
  let open Z in
  match t with
  | TSigned Ti8 -> geq n (of_int (-128)) && leq n (of_int 127)
  | TSigned Ti16 -> geq n (of_int (-32768)) && leq n (of_int 32767)
  | TSigned Ti32 -> fits_int32 n
  | TSigned Ti64 -> fits_int64 n
  | TSigned Ti128 ->
      let min_i128 = neg (shift_left one 127) in
      let max_i128 = sub (shift_left one 127) one in
      geq n min_i128 && leq n max_i128
  | TUnsigned Tu8 -> geq n zero && leq n (of_int 255)
  | TUnsigned Tu16 -> geq n zero && leq n (of_int 65535)
  | TUnsigned Tu32 -> fits_int32_unsigned n
  | TUnsigned Tu64 -> fits_int64_unsigned n
  | TUnsigned Tu128 -> geq n zero && leq n Z.(sub (shift_left one 128) one)

let fits_in_float_ty (n : float) (t : Typed_ast.float_ty) : bool =
  match t with
  | Tf32 ->
      (* IEEE-754 f32 range limits *)
      let max_f32 = 3.40282347e38 in
      let min_f32 = -.max_f32 in
      (not (Float.is_nan n))
      && Float.is_finite n && n >= min_f32 && n <= max_f32
  | Tf64 -> true

let int_in_float (n : Z.t) (t : Typed_ast.float_ty) : bool =
  let open Z in
  match t with
  | Tf32 ->
      (* f32 has 24 bits of precision *)
      leq (abs n) (shift_left one 24)
  | Tf64 ->
      (* f64 has 53 bits of precision *)
      leq (abs n) (shift_left one 53)

let rec eval_const_exp (e : exp node) : Z.t option =
  match e.elt with
  | Int i -> Some i
  | Bop (Add, e1, e2) -> (
      match (eval_const_exp e1, eval_const_exp e2) with
      | Some v1, Some v2 -> Some Z.(v1 + v2)
      | _ -> None)
  | Bop (Sub, e1, e2) -> (
      match (eval_const_exp e1, eval_const_exp e2) with
      | Some v1, Some v2 -> Some Z.(v1 - v2)
      | _ -> None)
  | Bop (Mul, e1, e2) -> (
      match (eval_const_exp e1, eval_const_exp e2) with
      | Some v1, Some v2 -> Some Z.(v1 * v2)
      | _ -> None)
  | Bop (Div, e1, e2) -> (
      match (eval_const_exp e1, eval_const_exp e2) with
      | Some v1, Some v2 -> Some Z.(v1 / v2)
      | _ -> None)
  | Bop (Mod, e1, e2) -> (
      match (eval_const_exp e1, eval_const_exp e2) with
      | Some v1, Some v2 -> Some Z.(v1 mod v2)
      | _ -> None)
  | Bop (Pow, e1, e2) -> (
      match (eval_const_exp e1, eval_const_exp e2) with
      | Some v1, Some v2 -> Some Z.(pow v1 (to_int v2))
      | _ -> None)
  | _ -> None
