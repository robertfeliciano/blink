#pragma once
extern "C" {
#include <caml/mlvalues.h>
}
class Ty;
Ty* convert_ty(value ocaml_ty);

class Expr;
Expr* convert_expr(value ocaml_expr);

class Stmt;
Stmt* convert_stmt(value ocaml_stmt);
