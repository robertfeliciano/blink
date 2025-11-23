#include <iostream>

#include <caml/mlvalues.h>
#include <caml/memory.h>

#include <bridge/prog.h>
#include <util/print.h>
#include <codegen/generator.h>

using namespace std;

extern "C" {
    value convert_caml_ast(value p) {
        CAMLparam1(p);
        Program prog = convert_program(p);
        std::cout << programToString(prog) << std::endl;
        Generator gen;
        gen.configureTarget();
        gen.codegenProgram(prog);
        gen.dumpLL("new_output.ll");
        CAMLreturn(Val_unit);
    }
}