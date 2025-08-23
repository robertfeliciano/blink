module Range = Util.Range

type 'a node = { elt: 'a; loc: Range.t }

type id = string

type sint = 
| Ti8
| Ti16
| Ti32
| Ti64
| Ti128
[@@deriving show]

type uint = 
| Tu8
| Tu16
| Tu32
| Tu64
| Tu128
[@@deriving show]

type float_ty =
| Tf32
| Tf64
[@@deriving show]

type int_ty = 
| TSigned of sint
| TUnsigned of uint
[@@deriving show]

type ty =
| TBool
| TInt of int_ty
| TFloat of float_ty
| TRef of ref_ty
and ref_ty =
| RString
| RArray of ty
(* | RClass of id *)
| RFun of ty list * ret_ty
and ret_ty =
| RetVoid
| RetVal of ty
[@@deriving show]

type unop = 
| Neg
| Not
[@@deriving show]

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

type aop =
| Eq
| PluEq
| MinEq
| TimEq
| DivEq
| AtEq
| PowEq
| ModEq
[@@deriving show]

type exp = 
  | Bool of bool
  | Int of int64 * int_ty
  | TFloat of float * float_ty
  | Str of string 
  | Id of id
  | Call of exp node * exp node list * ty
  | Bop of binop * exp node * exp node * ty
  | Uop of unop * exp node * ty
  | Index of exp node * exp node * ty
  | Array of exp node list * ty
  | Range of exp node * exp node * bool