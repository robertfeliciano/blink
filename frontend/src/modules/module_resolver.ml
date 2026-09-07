(** Resolve each module in its own scope before combining declarations.
    Returning Ast.program is intentional: modules disappear before lowering. *)
open Module_model

(* TODO(modules-07): Build per-module public-export/private-local tables plus
   import aliases; resolve qualified functions, classes and class types.
   Only declarations marked with the top-level export keyword are importable.
   Recursively rewrite with lexical scope.
   Never rename an Id solely because its spelling matches a top-level function.
   Walk initializers, lambdas, ternaries, annotations and class signatures too. *)
(* TODO(modules-08): Validate the resolved representation with existing typing:
   preserve nominal class identity, method/constructor behavior, partial calls
   and declaration locations. Reject unimported names before merging modules. *)
(* TODO(modules-09): Audit desugaring and codegen symbol consumers. Preserve
   external symbols and root main; qualify class ownership before existing
   method mangling. Do not add a module constructor to the positional FFI. *)
let resolve (_ : graph) : (Ast.program, diagnostic) result = pending "07"
