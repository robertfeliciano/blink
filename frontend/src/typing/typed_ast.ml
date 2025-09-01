type id = string
type sint = Ti8 | Ti16 | Ti32 | Ti64 | Ti128 [@@deriving show]
type uint = Tu8 | Tu16 | Tu32 | Tu64 | Tu128 [@@deriving show]
type float_ty = Tf32 | Tf64 [@@deriving show]
type int_ty = TSigned of sint | TUnsigned of uint [@@deriving show]

type ty = TBool | TInt of int_ty | TFloat of float_ty | TRef of ref_ty

and ref_ty =
  | RString
  | RArray of ty * int64
  | RRange of ty * ty
  (* | RClass of id *)
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
  | Call of exp * exp list * ty
  | Bop of binop * exp * exp * ty
  | Uop of unop * exp * ty
  | Index of exp * exp * ty
  | Array of exp list * ty * int64
  | Cast of exp * ty
  | Range of exp * exp * bool

type vdecl = id * ty * exp * bool

type stmt =
  | Assn of exp * aop * exp
  | Decl of vdecl (* includes whether it was declared as constant or not *)
  | Ret of exp option
  | SCall of exp * exp list
  | If of exp * block * block
  | For of id * exp * exp * block
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

type program = Prog of fdecl list
