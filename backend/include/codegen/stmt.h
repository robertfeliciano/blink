#pragma once

#include <iostream>
#include <string>

#include "llvm/IR/Value.h"

#include <bridge/stmt.h>
#include <codegen/exp.h>

using llvm::Value;

struct Generator;

class StmtToLLVisitor {
    Generator& gen;

public:
    explicit StmtToLLVisitor(Generator& g) : gen(g) {}

    llvm::Value* operator()(const Assn& s);
    llvm::Value* operator()(const VDecl& s);
    llvm::Value* operator()(const Ret& s);
    llvm::Value* operator()(const SCall& s);
    llvm::Value* operator()(const If& s);
    llvm::Value* operator()(const While& s);
    llvm::Value* operator()(const Break& s);
    llvm::Value* operator()(const Continue& s);
};