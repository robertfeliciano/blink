#pragma once

#include <bridge/decl.h>
#include <caml/mlvalues.h>
#include <string>
#include <vector>

enum class BlinkOptimizationLevel { O0, O1, O2, O3 };

struct InterfaceField {
    std::string fieldName;
    Ty          ftyp;
    bool        fconst;
};

struct InterfaceDecl {
    std::string                 iname;
    std::vector<Proto>          protos;
    std::vector<InterfaceField> fields;
};

struct Program {
    BlinkOptimizationLevel     optimizationLevel;
    std::vector<FDecl>         functions;
    std::vector<CDecl>         classes;
    std::vector<Proto>         protos;
    std::vector<GDecl>         globals;
    std::vector<InterfaceDecl> interfaces;
};

Program convert_program(value v);
