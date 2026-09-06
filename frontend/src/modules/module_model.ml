(** Draft contracts for the module implementation. Not wired into Compiler. *)

(* TODO(modules-01): Confirm the MVP rules in docs/modules.md before filling in
   the stubs. These types describe compiler bookkeeping, not runtime values. *)
type module_id = string list

type config = { project_root : string; stdlib_root : string option }

(* TODO(modules-02): Move the import syntax into Ast when extending Ast.program.
   Replace this draft record with an alias to that authoritative AST type. *)
type import = {
  path : module_id;
  alias : string option;
  loc : Util.Range.t;
}

type source = {
  id : module_id;
  filename : string;
  imports : import list;
  program : Ast.program;
}

type graph = { entry : module_id; dependency_order : source list }

type diagnostic = { loc : Util.Range.t; message : string }

let pending step =
  Error
    {
      loc = Util.Range.norange;
      message = "Module skeleton: implement TODO(modules-" ^ step ^ ")";
    }
