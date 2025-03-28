module Range = Util.Range

type 'a node = { elt: 'a; loc: Range.t }

let no_loc x = { elt = x; loc = Range.norange }

type id = string

type sint = 
| Ti8
| Ti16
| Ti32
| Ti64
| Ti128

type uint = 
| Tu8
| Tu16
| Tu32
| Tu64
| Tu128

type float_ty =
| Tf32
| Tf64

type int_ty = 
| TSigned of sint
| TUnsigned of uint

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

type unop = 
| Neg
| Not

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

type aop =
| Eq
| PluEq
| MinEq
| TimEq
| DivEq
| AtEq
| PowEq
| ModEq

type exp = 
| Bool of bool
| Int of int64
| Float of float
| Str of string
| Id of id
| Call of exp node * exp node list
| Bop of binop * exp node * exp node
| Uop of unop * exp node
| Index of exp node * exp node
| Array of exp node list
| Range of exp node * exp node * bool (* includes whether right bound is inclusive *)

(* TODO add class decl to exp type*)

type vdecl = id * ty option * exp node * bool

type stmt = 
| Assn of exp node * aop * exp node
| Decl of vdecl (* includes whether it was declared as constant or not *)
| Ret of exp node option
| SCall of exp node * exp node list
| If of exp node * block * block 
| For of id node * exp node * exp node option * block
| While of exp node * block
| Break
| Continue
and block = stmt node list

type gdecl = { name : id; init : exp node }

type fdecl = { frtyp : ret_ty ; fname : id; args : (ty * id) list; mutable body : block }

type field = { fieldName : id; ftyp : ty }

(* type cdecl = id * (field list * fdecl list) *)

type decl =
| Gvdecl of gdecl node
| Gfdecl of fdecl node
(* | Gcdecl of cdecl node *)

type prog = decl list
