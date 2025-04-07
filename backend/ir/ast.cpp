#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <iostream>
#include <vector>
#include <string>
using namespace std;




extern "C" {
    value convert_caml_ast(value ocaml_ast) {
        CAMLparam1(ocaml_ast);
        Prog* prog = convert_ast(ocaml_ast);
        prog->print();
        delete ast;
        CAMLreturn(Val_unit);
    }
}