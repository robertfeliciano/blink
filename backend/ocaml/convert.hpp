#pragma once
extern "C" {
#include <caml/mlvalues.h>
}
class Ty;
Ty* convert_ty(value ocaml_ty);
// Add others: Expr* convert_expr(...), etc.
