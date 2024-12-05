open Ast

let sprintf = Printf.sprintf

let string_of_list (f: 'a -> string) (lst: 'a list) : string =
  sprintf "[ %s ]" (String.concat " ; " (List.map f lst))
(* 
let string_of_node (f: 'a -> string) (n: 'a node) : string =
  sprintf "{ loc: %s ; elt: %s }" (Range.string_of_range n.loc) (f n.elt) *)

let string_of_node (f: 'a -> string) (n: 'a node) : string =
  sprintf "%s" (f n.elt)

let rec string_of_type = function 
  | TBool -> "bool"
  | TInt -> "int" 
  | TFloat -> "float"
  | TRef a -> 
    match a with
      | RString -> "string"
      | RArray t -> string_of_type t |> sprintf "array of %s"
      | _ -> "not supported yet"

let string_of_retty = function
  | RetVoid -> "void"
  | RetVal v -> string_of_type v

let string_of_args: ((ty*id) list) -> string = 
  string_of_list (fun (t, i) -> sprintf "%s: %s" i (string_of_type t))

let string_of_aop = function
  | Eq -> "="
  | PluEq -> "+="
  | MinEq -> "-="
  | TimEq -> "*="
  | DivEq -> "/="
  | AtEq -> "@="
  | PowEq -> "**="

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Dot -> "@"
  | Mod -> "%"
  | Pow -> "**"
  | Eqeq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | And -> "and"
  | Or -> "or"

let string_of_unop = function
  | Neg -> "-"
  | Not -> "not"

let rec string_of_exp (e: exp node) : string =
  string_of_node string_of_exp_helper e
and string_of_exp_helper (e:exp) : string = 
  match e with 
  | Bool b -> if b then "true" else "false"
  | Int i -> string_of_int @@ Int64.to_int i
  | Float f -> string_of_float @@ f
  | Str s -> s
  | Id id -> id
  | Call (func, arglist) -> sprintf "%s(%s)" (string_of_exp func) (string_of_list string_of_exp arglist)
  | Bop (b, lop, rop) -> sprintf "(%s %s %s)" (string_of_exp lop) (string_of_binop b) (string_of_exp rop)
  | Uop (u, op) -> sprintf "(%s %s)" (string_of_unop u) (string_of_exp op)
  | Index (lst, offset) -> sprintf "%s[%s]" (string_of_exp lst) (string_of_exp offset)
  | Array (elems) -> sprintf "%s" (string_of_list string_of_exp elems)
  | Range (left, right, inc) -> sprintf "%s..%s%s" (string_of_exp left) (if inc then "=" else "") (string_of_exp right)

let rec string_of_id (i: id node) : string = 
  string_of_node string_of_id_helper i
and string_of_id_helper (i: id) : string = i

let rec string_of_stmt (s: stmt node) : string = 
  string_of_node string_of_stmt_helper s
and string_of_body (b: block) : string =
  string_of_list string_of_stmt b
and string_of_stmt_helper (s: stmt) : string = 
  match s with 
  | Assn(lhs, op, rhs) -> sprintf "%s %s %s" (string_of_exp lhs) (string_of_aop op) (string_of_exp rhs)
  | Decl (name, typ, rhs, const) -> sprintf "%s%s: %s := %s" (if const then "const " else "") (name) (
    match typ with 
    | Some t -> string_of_type t
    | None -> "inferred"
  ) (string_of_exp rhs)
  | Ret v -> sprintf "return %s" (
    match v with 
    | Some e -> string_of_exp e
    | None -> ""
  )
  | SCall (func, arglist) -> sprintf "%s(%s)" (string_of_exp func) (string_of_list string_of_exp arglist)
  | If (cond, tbr, ebr) -> sprintf "if %s \n\t %s \n\t%s\n" (string_of_exp cond) (string_of_body tbr) (string_of_body ebr) 
  | For (iter, range, b) -> sprintf "for %s in %s \n\t%s\n" (string_of_id iter) (string_of_exp range) (string_of_body b)
  | While (cond, b) ->  sprintf "while %s \n\t%s\n" (string_of_exp cond) (string_of_body b)
  | Break -> "break"
  | Continue -> "continue"

let string_of_fdecl_helper (f: fdecl) : string = 
  sprintf "name: %s; args: %s; returns: %s; body: \n\t%s\n" 
  (f.fname) (string_of_args f.args) 
  (string_of_retty f.frtyp) (string_of_body f.body)

let string_of_fdecl (f: fdecl node) : string =
  string_of_node string_of_fdecl_helper f

let string_of_decl: decl -> string = function
  | Gfdecl f -> sprintf "Fun %s" (string_of_fdecl f)
  | _ -> sprintf "not implemented yet"

let string_of_prog: prog -> string =
  string_of_list string_of_decl


let print_ast ast = Printf.printf "\n%s\n\n" (string_of_prog ast)