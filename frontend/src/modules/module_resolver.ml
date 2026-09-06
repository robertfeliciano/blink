open Module_model

(* TODO(modules-07): Build per-module exports/import aliases, resolve qualified
   functions AND type names, and recursively rewrite with lexical scope.
   Never rename an Id solely because its spelling matches a global function.
   Walk initializers, lambdas, ternaries, annotations and class signatures too. *)
(* TODO(modules-08): Validate the resolved representation with existing typing:
   preserve nominal class identity, method/constructor behavior, partial calls
   and declaration locations. Reject unimported names before merging modules. *)
(* TODO(modules-09): Audit desugaring and codegen symbol consumers. Preserve
   external symbols and root main; qualify class ownership before existing
   method mangling. Do not add a module constructor to the positional FFI. *)
let resolve (_ : graph) : (Ast.program, diagnostic) result = pending "07"
