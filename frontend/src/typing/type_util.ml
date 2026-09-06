open Ast
module Printer = Pprint_typed_ast

exception TypeError of string

let make_error_underline c1 c2 =
  let start_col = max 1 c1 in
  let end_col = max (start_col + 1) c2 in
  let spaces = String.make (start_col - 1) ' ' in
  let carets = String.make (end_col - start_col) '^' in
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

let underline_range ~offset ~start_col ~end_col line_len =
  let s = match start_col with None -> 1 | Some c -> c in
  let e = match end_col with None -> line_len + 1 | Some c -> c in
  make_error_underline (s + offset) (e + offset)

let type_error (l : 'a node) err =
  let filename, (l1, c1), (l2, c2) = l.loc in

  let line_indicator ln = Printf.sprintf "%4d | " ln in

  let get ln =
    let line = get_line filename ln in
    let indicator = line_indicator ln in
    let offset = String.length indicator in
    (indicator, line, offset)
  in

  let build ln =
    let indicator, line, offset = get ln in
    let underline =
      if l1 = l2 then
        underline_range ~offset ~start_col:(Some c1) ~end_col:(Some c2)
          (String.length line)
      else if ln = l1 then
        underline_range ~offset ~start_col:(Some c1) ~end_col:None
          (String.length line)
      else if ln = l2 then
        underline_range ~offset ~start_col:None ~end_col:(Some c2)
          (String.length line)
      else
        underline_range ~offset ~start_col:None ~end_col:None
          (String.length line)
    in
    indicator ^ line ^ "\n" ^ underline
  in

  let lines =
    let span = max 1 (l2 - l1 + 1) in
    if span <= 5 then List.init span (fun i -> build (l1 + i))
    else [ build l1; build (l1 + 1); "     | ..."; build (l2 - 1); build l2 ]
  in

  raise
    (TypeError
       (Printf.sprintf "Error at %s:%d:%d:\n%s\n%s" filename l1 c1
          (String.concat "\n" lines) err))

let type_failure err = raise (TypeError (Printf.sprintf "Error: %s" err))

let type_warning (l : 'a node) err =
  let _, (s, e), _ = l.loc in
  Printf.eprintf "[%d, %d] Warning: %s" s e err

let rec map2_exact f xs ys =
  match (xs, ys) with
  | [], [] -> Some []
  | x :: xs, y :: ys ->
      let mapped = f x y in
      Option.map (fun rest -> mapped :: rest) (map2_exact f xs ys)
  | _ -> None

let rec lists_equal_exact equal xs ys =
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> equal x y && lists_equal_exact equal xs ys
  | _ -> false

let check_body_return_completeness (node : 'a node)
    (ret_ty : Typed_ast.ret_ty) ~(does_ret : bool) ~(body_kind : string) : unit =
  match ret_ty with
  | Typed_ast.RetVoid -> ()
  | Typed_ast.RetVal _ when does_ret -> ()
  | Typed_ast.RetVal _ ->
      type_error node ("Missing return statement in " ^ body_kind ^ ".")

let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with
  | TInt _ | TFloat _ | TBool -> ()
  | TRef r -> typecheck_rty l tc r

and typecheck_rty (l : 'a Ast.node) (tc : Tctxt.t) (r : Ast.ref_ty) : unit =
  match r with
  | RString -> ()
  | RArray (t, sz) ->
      if Z.lt sz (Z.of_int 0) then type_error l "negative length specified"
      else if not (Z.fits_int sz) then
        type_error l "array length is too large for this target"
      else typecheck_ty l tc t
  | RClass c ->
      if None = Tctxt.lookup_class_option c tc then
        type_error l "class undefined"
  | RFun (tl, rt) ->
      List.iter (typecheck_ty l tc) tl;
      typecheck_ret_ty l tc rt
  | RGeneric _ -> type_error l "Generic types are not supported."

and typecheck_ret_ty (l : 'a Ast.node) (tc : Tctxt.t) (rt : Ast.ret_ty) : unit =
  match rt with RetVoid -> () | RetVal t -> typecheck_ty l tc t

let validate_and_convert_ty (node : 'a Ast.node) (tc : Tctxt.t) (ty : Ast.ty) :
    Typed_ast.ty =
  typecheck_ty node tc ty;
  Conversions.convert_ty ty

let validate_and_convert_ret_ty (node : 'a Ast.node) (tc : Tctxt.t)
    (ret_ty : Ast.ret_ty) : Typed_ast.ret_ty =
  typecheck_ret_ty node tc ret_ty;
  Conversions.convert_ret_ty ret_ty

let validate_and_convert_signature (node : 'a Ast.node) (tc : Tctxt.t)
    (args : (Ast.ty * id) list) (ret_ty : Ast.ret_ty) :
    (Typed_ast.ty * id) list * Typed_ast.ret_ty =
  let typed_args =
    List.map (fun (ty, id) -> (validate_and_convert_ty node tc ty, id)) args
  in
  (typed_args, validate_and_convert_ret_ty node tc ret_ty)

let validate_and_convert_function_ty node tc args ret_ty =
  let typed_args, typed_ret_ty =
    validate_and_convert_signature node tc args ret_ty
  in
  Typed_ast.TRef (RFun (List.map fst typed_args, typed_ret_ty))

let sint_width : Typed_ast.sint -> int = function
  | Ti8 -> 8
  | Ti16 -> 16
  | Ti32 -> 32
  | Ti64 -> 64
  | Ti128 -> 128

let uint_width : Typed_ast.uint -> int = function
  | Tu8 -> 8
  | Tu16 -> 16
  | Tu32 -> 32
  | Tu64 -> 64
  | Tu128 -> 128

let int_width : Typed_ast.int_ty -> int = function
  | TSigned s -> sint_width s
  | TUnsigned u -> uint_width u

let signed_of_width = function
  | 8 -> Some Typed_ast.Ti8
  | 16 -> Some Ti16
  | 32 -> Some Ti32
  | 64 -> Some Ti64
  | 128 -> Some Ti128
  | _ -> None

let unsigned_of_width = function
  | 8 -> Some Typed_ast.Tu8
  | 16 -> Some Tu16
  | 32 -> Some Tu32
  | 64 -> Some Tu64
  | 128 -> Some Tu128
  | _ -> None

let next_signed_width width =
  List.find_opt (fun candidate -> candidate > width) [ 8; 16; 32; 64; 128 ]

let widest_int (ity1 : Typed_ast.int_ty) (ity2 : Typed_ast.int_ty) (n : 'a node)
    : Typed_ast.int_ty =
  let invalid () = type_error n "Invalid integer type during promotion." in
  match (ity1, ity2) with
  | TSigned s1, TSigned s2 -> (
      match signed_of_width (max (sint_width s1) (sint_width s2)) with
      | Some s -> TSigned s
      | None -> invalid ())
  | TUnsigned u1, TUnsigned u2 -> (
      match unsigned_of_width (max (uint_width u1) (uint_width u2)) with
      | Some u -> TUnsigned u
      | None -> invalid ())
  | TSigned s, TUnsigned u | TUnsigned u, TSigned s ->
      let signed_width = sint_width s in
      let unsigned_width = uint_width u in
      let target_width =
        if signed_width > unsigned_width then Some signed_width
        else next_signed_width unsigned_width
      in
      (match Option.bind target_width signed_of_width with
      | Some target -> TSigned target
      | None ->
          type_error n
            ("Cannot safely promote mixed signed and unsigned integers with "
           ^ string_of_int signed_width ^ "-bit and "
           ^ string_of_int unsigned_width ^ "-bit widths."))

let widest_float (fty1 : Typed_ast.float_ty) (fty2 : Typed_ast.float_ty) :
    Typed_ast.float_ty =
  if fty1 = fty2 then fty1 else Tf64

let meet_number (n : 'a node) : Typed_ast.ty * Typed_ast.ty -> Typed_ast.ty =
  function
  | TInt i1, TInt i2 -> TInt (widest_int i1 i2 n)
  | (TFloat Tf64, TInt _ | TInt _, TFloat Tf64) -> TFloat Tf64
  | (TFloat Tf32, TInt i | TInt i, TFloat Tf32) ->
      (* f32 can represent every integer of up to 16 bits exactly. Wider integer
         operands promote the operation to f64 to avoid needless precision loss. *)
      if int_width i <= 16 then TFloat Tf32 else TFloat Tf64
  | TFloat f1, TFloat f2 -> TFloat (widest_float f1 f2)
  | _ -> type_error n "unreachable state: meeting non-numbers."

let is_number (t : Typed_ast.ty) : bool =
  match t with Typed_ast.TInt _ | Typed_ast.TFloat _ -> true | _ -> false

let is_float (t : Typed_ast.ty) : bool =
  match t with Typed_ast.TFloat _ -> true | _ -> false

let is_integer (t : Typed_ast.ty) : bool =
  match t with Typed_ast.TInt _ -> true | _ -> false

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
  | RFun (params1, ret1), RFun (params2, ret2) ->
      lists_equal_exact equal_ty params1 params2
      && equal_ret_ty ret1 ret2
  | RClass c1, RClass c2 -> String.equal c1 c2
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
  | TFloat _, TInt _ ->
      true
  | TRef t1', TRef t2' -> subtype_ref tc t1' t2'
  | _ -> false

and subtype_ref (tc : Tctxt.t) (t1 : Typed_ast.ref_ty) (t2 : Typed_ast.ref_ty) :
    bool =
  match (t1, t2) with
  | RString, RString -> true
  | RArray (t1', sz1), RArray (t2', sz2) -> sz1 = sz2 && subtype tc t1' t2'
  | RFun (pty1, rty1), RFun (pty2, rty2) ->
      let contrav_params = lists_equal_exact equal_ty pty2 pty1 in
      contrav_params && subtype_ret_ty tc rty1 rty2
  | _ -> false

and subtype_ret_ty (tc : Tctxt.t) (t1 : Typed_ast.ret_ty)
    (t2 : Typed_ast.ret_ty) : bool =
  match (t1, t2) with
  | RetVoid, RetVoid -> true
  | RetVal t1', RetVal t2' -> subtype tc t1' t2'
  | _ -> false

let fits_in_int_ty (n : Z.t) (t : Typed_ast.int_ty) : bool =
  match t with
  | TSigned signed_ty ->
      let limit = Z.shift_left Z.one (sint_width signed_ty - 1) in
      Z.geq n (Z.neg limit) && Z.lt n limit
  | TUnsigned unsigned_ty ->
      Z.sign n >= 0 && Z.numbits n <= uint_width unsigned_ty

let infer_integer_ty (n : Z.t) (e : exp node) : Typed_ast.int_ty =
  let candidates : Typed_ast.int_ty list =
    [
      TSigned Ti32;
      TUnsigned Tu32;
      TSigned Ti64;
      TUnsigned Tu64;
      TSigned Ti128;
      TUnsigned Tu128;
    ]
  in
  match List.find_opt (fits_in_int_ty n) candidates with
  | Some int_ty -> int_ty
  | None -> type_error e ("integer literal `" ^ Z.to_string n ^ "` too large")

let max_finite_f32 = Int32.float_of_bits 0x7f7fffffl

let float_is_representable_in_ty (n : float) (t : Typed_ast.float_ty) : bool =
  if not (Float.is_finite n) then false
  else
    match t with
    | Tf32 -> Float.abs n <= max_finite_f32
    | Tf64 -> true

let int_is_exactly_representable_in_float_ty (n : Z.t)
    (t : Typed_ast.float_ty) : bool =
  let precision, max_bits =
    match t with Tf32 -> (24, 128) | Tf64 -> (53, 1024)
  in
  let bits = Z.numbits n in
  bits = 0
  || (bits <= max_bits
     && (bits <= precision || Z.trailing_zeros n >= bits - precision))

let exact_nonnegative_int (node : 'a node) description value =
  if Z.sign value < 0 then type_error node (description ^ " cannot be negative")
  else if not (Z.fits_int value) then
    type_error node (description ^ " is too large for this target")
  else Z.to_int value

let rec eval_const_exp (e : exp node) : Z.t option =
  match e.elt with
  | Int i -> Some i
  | Bop (Add, e1, e2) -> eval_const_binop e1 e2 Z.add
  | Bop (Sub, e1, e2) -> eval_const_binop e1 e2 Z.sub
  | Bop (Mul, e1, e2) -> eval_const_binop e1 e2 Z.mul
  | Bop (Div, e1, e2) ->
      eval_const_binop e1 e2 (fun v1 v2 ->
          if Z.equal v2 Z.zero then
            type_error e2 "Division by zero in constant expression."
          else Z.div v1 v2)
  | Bop (Mod, e1, e2) ->
      eval_const_binop e1 e2 (fun v1 v2 ->
          if Z.equal v2 Z.zero then
            type_error e2 "Modulo by zero in constant expression."
          else Z.rem v1 v2)
  | Bop (Pow, e1, e2) ->
      eval_const_binop e1 e2 (fun v1 v2 ->
          Z.pow v1 (exact_nonnegative_int e2 "Exponent" v2))
  | Bop (Shl, e1, e2) ->
      eval_const_binop e1 e2 (fun v1 v2 ->
          Z.shift_left v1 (exact_nonnegative_int e2 "Shift amount" v2))
  | Bop (Lshr, e1, e2) ->
      eval_const_binop e1 e2 (fun v1 v2 ->
          Z.shift_right_trunc v1
            (exact_nonnegative_int e2 "Shift amount" v2))
  | Bop (Ashr, e1, e2) ->
      eval_const_binop e1 e2 (fun v1 v2 ->
          Z.shift_right v1 (exact_nonnegative_int e2 "Shift amount" v2))
  | Uop (Neg, e1) -> (
      match eval_const_exp e1 with
      | Some v1 -> Some Z.(mul v1 (of_int (-1)))
      | _ -> None)
  | Bop (BAnd, e1, e2) -> eval_const_binop e1 e2 Z.logand
  | Bop (BXor, e1, e2) -> eval_const_binop e1 e2 Z.logxor
  | Bop (BOr, e1, e2) -> eval_const_binop e1 e2 Z.logor
  | Uop (BNeg, e1) -> (
      match eval_const_exp e1 with Some v1 -> Some Z.(lognot v1) | _ -> None)
  | _ -> None

and eval_const_binop e1 e2 operator =
  match (eval_const_exp e1, eval_const_exp e2) with
  | Some v1, Some v2 -> Some (operator v1 v2)
  | _ -> None

let unexpected_ty expected e =
 fun received ->
  type_error e
    ("Expteced type " ^ Printer.show_ty expected ^ ", received type " ^ received)

let check_expected_ty (expected : Typed_ast.ty option) (actual : Typed_ast.ty)
    (e : Ast.exp node) : unit =
  match expected with
  | Some t when not (equal_ty t actual) ->
      unexpected_ty t e (Printer.show_ty actual)
  | _ -> ()

let is_hardcoded = function Typed_ast.(Int _ | Float _) -> true | _ -> false

let default_of_ty = function
  | Typed_ast.(TInt _) -> Ok (TInt (TSigned Ti32))
  | Typed_ast.(TFloat _) -> Ok (TFloat Tf64)
  | _ -> Error (fun e -> type_error e "impossible state")

let default_step t enode =
  match t with
  | Typed_ast.(TInt int_ty) -> Typed_ast.Int (Z.of_int 1, int_ty)
  | Typed_ast.(TFloat float_ty) -> Typed_ast.Float (1.0, float_ty)
  | _ -> type_error enode "impossible state"

let is_const en =
  match en.elt with
  | Bool _ | Int _ | Float _ | Str _ | Array _ | TypedLambda _ -> true
  | Id _ | Call _ | Bop _ | Uop _ | Index _ | Cast _ | Proj _ | ObjInit _ | Null
  | Lambda _ | Conditional _ ->
      false
