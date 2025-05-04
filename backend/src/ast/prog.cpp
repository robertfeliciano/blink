#include <sstream>
#include <iostream>
#include <cstdio> 

#include <caml/mlvalues.h>
#include <caml/memory.h>

#include <ast/prog.h>

Program convert_program(value v) {
    CAMLparam1(v);
    // CAMLlocal1(decls);

    Program program;
    value decls = Field(v, 0);
    // decls = v;
    while (Is_block(decls)) {
        value decl = Field(decls, 0);
        program.push_back(convert_decl(decl));
        puts("beep");
        decls = Field(decls, 1);
        puts("boop");
    }
    puts("hehehehe");
    return program;
}

std::string programToString(const Program& prog) {
    std::ostringstream oss;
    for (const auto& decl : prog) {
        oss << declToString(decl) << "\n";
    }
    return oss.str();
}
