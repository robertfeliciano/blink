#include <sstream>
#include <iostream>

#include <caml/mlvalues.h>
#include <caml/memory.h>

#include <bridge/prog.h>
#include <bridge/decl.h>


Program convert_program(value prog) {
    // CAMLparam1(prog);

    Program program;

    value fdecls = Field(prog, 0);

    std::vector<FDecl> funs;

    while (fdecls != Val_emptylist) {
        value fn = Field(fdecls, 0);
        funs.push_back(convert_fdecl(fn));
        fdecls = Field(fdecls, 1);
    }

    value cdecls = Field(prog, 1);

    std::vector<CDecl> cls;

    while (cdecls != Val_emptylist) {
        value clazz = Field(cdecls, 0);
        cls.push_back(convert_cdecl(clazz));
        cdecls = Field(cdecls, 1);
    }

    program.functions = std::move(funs);
    program.classes = std::move(cls);

    return program;
}

std::string programToString(const Program& program) {
    std::ostringstream oss;
    for (const auto &c : program.classes) {
        oss << cdeclToString(c) << "\n\n";
    }
    for (const auto &f : program.functions) {
        oss << fdeclToString(f) << "\n\n";
    }
    return oss.str();
}