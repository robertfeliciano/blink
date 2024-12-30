open Ast
open Option

let sprintf = Printf.sprintf

let string_of_node ?(print_loc=false) (f: 'a -> string) (n: 'a node) : string =
  (f n.elt) ^ if print_loc then sprintf "<%s>"(Range.string_of_range n.loc) else ""

let string_of_list ?(sep=", ") (f: 'a -> string) (lst: 'a list) : string = 
  sprintf "%s" (String.concat sep (List.map f lst))

let rec string_of_type = function 
  | TBool -> "bool"
  | TInt -> "int" 
  | TFloat -> "float"
  | TRef a -> 
    match a with
      | RString -> "string"
      | RArray t -> string_of_type t |> sprintf "%s array"
      | _ -> "not supported yet"

let string_of_retty = function
  | RetVoid -> "void"
  | RetVal v -> string_of_type v

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
and string_of_exp_helper : exp -> string = function
  | Bool b -> if b then "true" else "false"
  | Int i -> string_of_int @@ Int64.to_int i
  | Float f -> string_of_float f
  | Str s -> "\"" ^ s ^ "\""
  | Id id -> id
  | Call (func, arglist) -> sprintf "%s(%s)" (string_of_exp func) (string_of_list string_of_exp arglist)
  | Bop (b, lop, rop) -> sprintf "(%s %s %s)" (string_of_exp lop) (string_of_binop b) (string_of_exp rop)
  | Uop (u, op) -> sprintf "(%s %s)" (string_of_unop u) (string_of_exp op)
  | Index (lst, offset) -> sprintf "%s[%s]" (string_of_exp lst) (string_of_exp offset)
  | Array (elems) -> sprintf "%s" (string_of_list string_of_exp elems)
  | Range (left, right, inc) -> sprintf "%s..%s%s" (string_of_exp left) (if inc then "=" else "") (string_of_exp right)

let rec string_of_stmt (s: stmt node) : string =
  sprintf "%s" @@ string_of_node ~print_loc:true string_of_stmt_helper s
and string_of_body (b: block) : string = 
  string_of_list ~sep:";\n" string_of_stmt b
and string_of_stmt_helper: stmt -> string = function
  | Assn(lhs, op, rhs) -> sprintf "%s %s %s" (string_of_exp lhs) (string_of_aop op) (string_of_exp rhs)
  | Decl(name, typ, rhs, const) -> sprintf "%s%s: %s := %s" (if const then "const " else "") (name) (
    match typ with
    | Some t -> string_of_type t
    | None -> "inferred"
  ) (string_of_exp rhs)
  | Ret v -> sprintf "return %s" (if is_some v then get v |> string_of_exp else "")
  | SCall (func, arglist) -> sprintf "%s(%s)" (string_of_exp func) (string_of_list string_of_exp arglist)
  | If (cond, tbr, ebr) -> sprintf "if %s \n\t%s\n\telse %s\n" (string_of_exp cond) (string_of_body tbr) (string_of_body ebr)
  | For (iter, range, b) -> sprintf "for %s in %s \n\t%s\n" (iter.elt) (string_of_exp range) (string_of_body b)
  | While (cond, b) ->  sprintf "while %s \n\t%s\n" (string_of_exp cond) (string_of_body b)
  | Break -> "break"
  | Continue -> "continue"

let string_of_fdecl_helper (f: fdecl) : string = 
  let name = sprintf "name: %s" f.fname in 
  let args = sprintf "args: %s" @@ 
    string_of_list 
    (fun (t, i) -> sprintf "%s: %s" i (string_of_type t)) 
    f.args in 
  let return_type = string_of_retty f.frtyp |> sprintf "returns: %s" in 
  let body = string_of_body f.body |> sprintf "body:\n%s" in 
  sprintf "\n%s\n%s\n%s\n%s\n" name args return_type body

let string_of_fdecl (f: fdecl node) : string = 
  string_of_node string_of_fdecl_helper f

let string_of_decl: decl -> string = function
  | Gfdecl f -> string_of_fdecl f 
  | _ -> "not implemented yet"

let string_of_prog (ast: prog) : string =
  string_of_list ~sep:";\n\n" string_of_decl ast 

let print_ast ast = Printf.printf "\n%s\n" (string_of_prog ast) 