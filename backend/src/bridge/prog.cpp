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

    for (value xs = Field(prog, 4); xs != Val_emptylist; xs = Field(xs, 1)) {
        value decl = Field(xs, 0);
        InterfaceDecl interface;
        interface.iname = String_val(Field(decl, 0));
        for (value ps = Field(decl, 1); ps != Val_emptylist; ps = Field(ps, 1))
            interface.protos.push_back(convert_proto(Field(ps, 0)));
        program.interfaces.push_back(std::move(interface));
    }
    for (value xs = Field(prog, 5); xs != Val_emptylist; xs = Field(xs, 1)) {
        value impl = Field(xs, 0);
        InterfaceImpl implementation;
        implementation.cname = String_val(Field(impl, 0));
        implementation.iname = String_val(Field(impl, 1));
        for (value ms = Field(impl, 2); ms != Val_emptylist; ms = Field(ms, 1))
            implementation.methods.emplace_back(String_val(Field(ms, 0)));
        program.implementations.push_back(std::move(implementation));
    }
    return program;
}
