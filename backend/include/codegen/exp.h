#pragma once

#include "llvm/IR/Value.h"

#include "ast/exp.h"

struct Generator;

class ExpToLLVisitor {
    Generator& gen;

public:
    explicit ExpToLLVisitor(Generator& g) : gen(g) {}

    llvm::Value* operator()(const EInt& e);
    llvm::Value* operator()(const EBool& e);
    llvm::Value* operator()(const EVar& e);
    llvm::Value* operator()(const EFloat& e);
    llvm::Value* operator()(const EStr& e);
    llvm::Value* operator()(const ECall& e);
    llvm::Value* operator()(const EBop& e);
    llvm::Value* operator()(const EUop& e);
    llvm::Value* operator()(const EIndex& e);
    llvm::Value* operator()(const EArray& e);
    llvm::Value* operator()(const ERange& e);
};