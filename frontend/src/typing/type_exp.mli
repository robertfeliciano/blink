(* type_exp.mli *)

val type_exp :
  ?expected:Typed_ast.ty ->
  Tctxt.t ->
  Ast.exp Ast.node ->
  Typed_ast.exp * Typed_ast.ty
(** Typecheck an expression.
    @param expected an optional expected type to guide inference
    @param tc the typing context
    @param e the expression node
    @return the typed expression and its inferred type *)

val type_func :
  Ast.exp Ast.node list ->
  Typed_ast.ty ->
  bool ->
  Tctxt.t ->
  (Typed_ast.exp list * Typed_ast.ret_ty, string) result
(** Typecheck a function call.
    @param args list of argument expression nodes
    @param ftyp the function type (must be TRef (RFun ...))
    @param from_exp
      whether the function is called from an expression context (true) or a
      statement context (false)
    @param tc the typing context
    @return Ok (typed arguments, return type) or Error (a string message) *)

val type_method :
  Ast.exp ->
  Ast.exp Ast.node list ->
  bool ->
  Tctxt.t ->
  (Typed_ast.exp * Typed_ast.exp list * Typed_ast.ret_ty, string) result
(** Typecheck a method call.
    @param proj the projected expression (object and method name)
    @param args the argument expression nodes
    @param from_exp
      whether the function is called from an expression context (true) or a
      statement context (false)
    @param tc the typing context
    @return
      Ok (typed object, typed arguments, return type) or Error (a string
      message) *)
