#pragma once

#include <vector>
#include <string>

#include <caml/mlvalues.h>

#include <bridge/decl.h>

struct Program {
    std::vector<FDecl> functions;
    std::vector<CDecl> classes;
};

Program convert_program(value v);
