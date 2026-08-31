open Typed_ast
module Printer = Pprint_typed_ast

type ctxt = (id * (ty * bool)) list (* id -> type and is_const *)
type method_header = id * ret_ty * (ty * id) list

type class_ctxt =
  (id * ((id * ty * bool * bool) list * method_header list)) list
(* class_ctxt : {
    cname  : id
    fields : id * field_type * is_const * is_init
    mthds  : method_header list
} list
 
*)

type t = {
  locals : ctxt;
  globals : ctxt;
  classes : class_ctxt;
  protos : ctxt; (* id -> type and defined_yet *)
}

let empty = { locals = []; globals = []; classes = []; protos = [] }

let show_tys tctxt =
  String.concat "\n"
    (List.map
       (fun (i, (t, is_const)) ->
         Printf.sprintf "%s: %s %s" i
           (if is_const then "cosnt" else "var")
           (Printer.show_ty t))
       tctxt)

let show_classes tc =
  "Classes:\n"
  ^ String.concat "\n"
      (List.map
         (fun (cname, (_fields, _mthds)) -> Printf.sprintf "%s" cname)
         tc.classes)

let show_tctxt tc =
  Printf.sprintf "Types:\n%s\n%s" (show_tys tc.locals) (show_classes tc)

(* locals ------------------------------------------------------------------- *)
let add_local (c : t) (id : id) (bnd : ty * bool) : t =
  { c with locals = (id, bnd) :: c.locals }

let lookup_local_option id c : (ty * bool) option =
  List.assoc_opt id c.locals

(* globals ------------------------------------------------------------------ *)
let add_global (c : t) (id : id) (bnd : ty * bool) : t =
  { c with globals = (id, bnd) :: c.globals }

let set_proto (c : t) (id : id) (bnd : ty * bool) : t =
  { c with protos = (id, bnd) :: List.remove_assoc id c.protos }

let lookup_global_option id c : (ty * bool) option =
  List.assoc_opt id c.globals

let lookup_proto_option id c : (ty * bool) option =
  List.assoc_opt id c.protos

let lookup_option id c : (ty * bool) option =
  match lookup_local_option id c with
  | None -> (
      match lookup_global_option id c with
      | None -> lookup_proto_option id c
      | Some x -> Some x)
  | Some x -> Some x

let add_class c id fields funs =
  { c with classes = (id, (fields, funs)) :: c.classes }

let set_class c id fields funs =
  {
    c with
    classes = (id, (fields, funs)) :: List.remove_assoc id c.classes;
  }

let lookup_class_option id c =
  List.assoc_opt id c.classes

let lookup_field_option c_name f_name c =
  let rec lookup_field_aux f_name l =
    match l with
    | [] -> None
    | (field_name, ftyp, is_const, _) :: t ->
        if field_name = f_name then Some (ftyp, is_const)
        else lookup_field_aux f_name t
  in
  match lookup_class_option c_name c with
  | None -> None
  | Some (fields, _methods) -> lookup_field_aux f_name fields

let lookup_method_option c_name m_name c =
  let rec lookup_method_aux m_name l =
    match l with
    | [] -> None
    | (m, rtyp, args) :: t ->
        if m = m_name then Some (rtyp, args) else lookup_method_aux m_name t
  in
  match lookup_class_option c_name c with
  | None -> None
  | Some (_fields, methods) -> lookup_method_aux m_name methods
