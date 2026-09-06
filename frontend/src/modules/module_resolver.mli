(** Resolve each module in its own scope before combining declarations.
    Returning Ast.program is intentional: modules disappear before lowering. *)
val resolve :
  Module_model.graph -> (Ast.program, Module_model.diagnostic) result
