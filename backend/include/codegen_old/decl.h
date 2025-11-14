#pragma once

#include "llvm/IR/Verifier.h"

#include <ast/decl.h>
#include <ast/prog.h>

struct Generator;

class DeclToLLVisitor {
    Generator& gen;

public: 
    explicit DeclToLLVisitor(Generator& g) : gen(g) {}

    void operator()(const Node<GDecl>& g);
    void operator()(const Node<FDecl>& d);
    // void operator()(const GDecl& d) const;


    void codegenFunctionProtos(const Program& p);

private: 
    void codegenFunctionProto(const FDecl& f);
};