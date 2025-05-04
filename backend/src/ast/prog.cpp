#include <sstream>
#include <iostream>
#include <cstdio> 

#include <caml/mlvalues.h>
#include <caml/memory.h>

#include <ast/prog.h>

Program convert_program(value prog) {
    // CAMLparam1(prog);

    Program program;
    while (prog != Val_emptylist) {
        value head = Field(prog, 0);
        program.push_back(convert_decl(head));
        prog = Field(prog, 1);
    }
    return program;
}

std::string programToString(const Program& prog) {
    std::ostringstream oss;
    for (const auto& decl : prog) {
        oss << declToString(decl) << "\n";
    }
    return oss.str();
}
