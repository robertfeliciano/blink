#pragma once

#include "llvm/IR/Verifier.h"

#include <bridge/decl.h>
#include <bridge/prog.h>

struct Generator;

class DeclToLLVisitor {
    Generator& gen;

public: 
    explicit DeclToLLVisitor(Generator& g) : gen(g) {}

    void operator()(const CDecl& g);
    void operator()(const FDecl& d);


    void codegenFunctionPrototypes(const std::vector<FDecl>& fns);

private: 
    void codegenFunctionProto(const FDecl& fn);
};