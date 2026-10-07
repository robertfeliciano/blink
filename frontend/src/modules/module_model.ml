(** Compile-time module identities, source graphs, configuration and
    diagnostics. *)

(* These types describe compiler bookkeeping, not runtime values. *)
type module_id = string list
type config = { project_root : string; stdlib_root : string option }
type import = Ast.import Ast.node
type source = { id : module_id; filename : string; program : Ast.program }
type graph = { entry : module_id; dependency_order : source list }
type diagnostic = { loc : Util.Range.t; message : string }
type state = Visiting | Visited
