open Typed_ast

type ctxt = (id * ty) list
(* type class_ctxt = ( id *  field list) list *)

type t = {
  locals: ctxt;
  globals: ctxt;
  (* classes: class_ctxt; *)
}


(* let empty = { locals = []; globals = []; classes = [] } *)
let empty = { locals = []; globals = [] }

(* locals ------------------------------------------------------------------- *)
let add_local (c:t) (id:id) (bnd :  ty) : t = {c with locals = (id, bnd)::c.locals}
let lookup_local (id :  id) (c : t) :  ty = List.assoc id c.locals
let lookup_local_option id c :  ty option =
  try Some (List.assoc id c.locals) with Not_found -> None

(* globals ------------------------------------------------------------------ *)
let add_global (c:t) (id:id) (bnd: ty) : t = {c with globals = (id, bnd)::c.globals}
let lookup_global (id :  id) (c : t) :  ty = List.assoc id c.globals
let lookup_global_option id c :  ty option =
  try Some (List.assoc id c.globals) with Not_found -> None

(* general-purpose lookup: for local _or_ global *)
let lookup id c :  ty =
  match lookup_local_option id c with
  | None -> lookup_global id c
  | Some x -> x

let lookup_option id c :  ty option =
  match lookup_local_option id c with
  | None -> lookup_global_option id c
  | Some x -> Some x


(* classures --------------------------------------------------------------- *)
(* let add_class c id bnd = {c with classes=(id, bnd)::c.classes}
let lookup_class id c = List.assoc id c.classes

let lookup_class_option id c =
  try Some (lookup_class id c) with Not_found -> None

let lookup_field_option st_name f_name c =
  let rec lookup_field_aux f_name l =
    match l with
    | [] -> None
    | h :: t -> if h.fieldName = f_name then Some h.ftyp else lookup_field_aux f_name t in
  match lookup_class_option st_name c with
  | None -> None
  | Some (fields, _methods) -> lookup_field_aux f_name fields

let lookup_field st_name f_name c =
  match lookup_field_option st_name f_name c with
  | None -> failwith "classCtxt.lookup_field: Not found"
  | Some x -> x 



let lookup_method_option c_name m_name c =
  let rec lookup_method_aux m_name l = 
    match l with 
    | [] -> None
    | h :: t -> if h.fname = m_name then Some h else lookup_method_aux m_name t in 
  match lookup_class_option c_name c with 
  | None -> None
  | Some (_fields, methods) -> lookup_method_aux m_name methods
  *)
  