type id = string
type sint = Ti8 | Ti16 | Ti32 | Ti64 | Ti128 [@@deriving show]
type uint = Tu8 | Tu16 | Tu32 | Tu64 | Tu128 [@@deriving show]
type float_ty = Tf32 | Tf64 [@@deriving show]
type int_ty = TSigned of sint | TUnsigned of uint [@@deriving show]

type ty = TBool | TInt of int_ty | TFloat of float_ty | TRef of ref_ty

and ref_ty =
  | RString
  | RArray of ty * int64
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
  | Int of Z.t * int_ty
  | Float of float * float_ty
  | Str of string
  | Id of id
  | Call of exp * exp list * ty list * ty
  | Bop of binop * exp * exp * ty
  | Uop of unop * exp * ty
  | Index of exp * exp * ty
  | Array of exp list * ty * int64
  | Cast of exp * ty
  | Proj of exp * id * id (* x.y -> Proj(x, "y", type of x) *)
  | ObjInit of id * (id * exp) list
  | Lambda of (id * ty) list * ret_ty * block

and vdecl = id * ty * exp * bool
and ldecl = id * ref_ty * exp

and stmt =
  | Assn of exp * aop * exp * ty
  | LambdaDecl of ldecl
  | Decl of vdecl (* includes whether it was declared as constant or not *)
  | Ret of exp option
  | SCall of exp * exp list * ty list
  | If of exp * block * block
  | For of
      id
      * exp
      * exp
      * bool
      * exp
      * ty (* type of iterator, i.e. float, i32, u8, etc *)
      * block (* iterator, start, stop, incl, step, body *)
  | ForEach of id * exp * ty * block
  | While of exp * block
  | Break
  | Continue

and block = stmt list

type fdecl = {
  frtyp : ret_ty;
  fname : id;
  args : (ty * id) list;
  mutable body : block;
}

type field = { fieldName : id; ftyp : ty; init : exp }

type cdecl = {
  cname : id;
  impls : id list;
  fields : field list;
  methods : fdecl list;
}

type program = Prog of fdecl list * cdecl list [@@boxed]
