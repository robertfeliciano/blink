(** Draft contracts for the module implementation. Not wired into Compiler. *)

(* Section 01's contract is settled in docs/modules.md. These types describe
   compiler bookkeeping, not runtime values. Enforcement belongs to later steps. *)
type module_id = string list

type config = { project_root : string; stdlib_root : string option }

type import = Ast.import Ast.node

type source = {
  id : module_id;
  filename : string;
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
