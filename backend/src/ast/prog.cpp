#include <caml/mlvalues.h>

#include <ast/prog.h>

Program convert_program(value v) {
    Program program;
    value decls = Field(v, 0);
    while (decls != Val_emptylist) {
        value decl = Field(decls, 0);
        program.push_back(convert_decl(decl));
        decls = Field(decls, 1);
    }
    return program;
}


