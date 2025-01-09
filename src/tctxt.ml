open Ast

type ctxt = (id, ty) Hashtbl.t
type class_ctxt = (id, field list) Hashtbl.t

type env = {
  locals: ctxt;
  globals: ctxt;
  classes: class_ctxt;
}

let empty = { locals = Hashtbl.create 20; globals = Hashtbl.create 10; classes = Hashtbl.create 5 }

let add = Hashtbl.add

let add_local env id typ : env = 
  Hashtbl.add env.locals id typ ; env

let lookup_local env id : ty option = 
  Hashtbl.find_opt env.locals id

let add_global env id typ : env = 
  Hashtbl.add env.globals id typ ; env

let lookup_global env id : ty option = 
  Hashtbl.find_opt env.globals id

let lookup env id : ty option =
  match lookup_local env id with 
  | None -> lookup_global env id
  | real -> real

let exists env id : bool = 
  Option.is_some @@ lookup env id

let add_class env id fields : env = 
  Hashtbl.add env.classes id fields ; env

let lookup_class env id : field list option = 
  Hashtbl.find_opt env.classes id 

let lookup_field env cls f : ty option = 
  match lookup_class env cls with 
  | Some c -> Some (List.find (fun field -> field.fieldName = f) c).ftyp
  | None -> None