#include <bridge/decl.h>
#include <bridge/prog.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <iostream>
#include <sstream>
#include <stdexcept>

Program convert_program(value prog) {
    Program program;

    switch (Long_val(Field(prog, 0))) {
        case 0:
            program.optimizationLevel = BlinkOptimizationLevel::O0;
            break;
        case 1:
            program.optimizationLevel = BlinkOptimizationLevel::O1;
            break;
        case 2:
            program.optimizationLevel = BlinkOptimizationLevel::O2;
            break;
        case 3:
            program.optimizationLevel = BlinkOptimizationLevel::O3;
            break;
        default:
            throw std::runtime_error("Unknown Blink optimization level");
    }

    value fdecls = Field(prog, 1);

    std::vector<FDecl> funs;

    while (fdecls != Val_emptylist) {
        value fn = Field(fdecls, 0);
        funs.push_back(convert_fdecl(fn));
        fdecls = Field(fdecls, 1);
    }

    value cdecls = Field(prog, 2);

    std::vector<CDecl> cls;

    while (cdecls != Val_emptylist) {
        value clazz = Field(cdecls, 0);
        cls.push_back(convert_cdecl(clazz));
        cdecls = Field(cdecls, 1);
    }

    value pdecls = Field(prog, 3);

    std::vector<Proto> protos;

    while (pdecls != Val_emptylist) {
        value proto = Field(pdecls, 0);
        protos.push_back(convert_proto(proto));
        pdecls = Field(pdecls, 1);
    }

    program.functions = std::move(funs);
    program.classes   = std::move(cls);
    program.protos    = std::move(protos);

    return program;
}
