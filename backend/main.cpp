#include <iostream>

#include <caml/mlvalues.h>
#include <caml/memory.h>

#include <ast/prog.h>

using namespace std;

extern "C" {
    value convert_caml_ast(value p) {
        CAMLparam1(p);
        Program prog = convert_program(p);
        std::cout << programToString(prog) << std::endl;
        CAMLreturn(Val_unit);
    }
}