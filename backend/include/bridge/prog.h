#pragma once

#include <bridge/decl.h>
#include <caml/mlvalues.h>
#include <string>
#include <vector>

struct Program {
    std::vector<FDecl> functions;
    std::vector<CDecl> classes;
};

Program convert_program(value v);
