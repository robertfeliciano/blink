#pragma once

#include <bridge/decl.h>
#include <bridge/prog.h>

struct Generator;

class DeclToLLVisitor {
    Generator& gen;

public: 
    explicit DeclToLLVisitor(Generator& g) : gen(g) {}

    void codegenCDecl(const CDecl& g);
    void codegenFDecl(const FDecl& d);


    void codegenFunctionPrototypes(const std::vector<FDecl>& fns);

private: 
    void codegenFunctionProto(const FDecl& fn);
};