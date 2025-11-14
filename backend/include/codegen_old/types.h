#pragma once

#include "llvm/IR/Constants.h"
#include "llvm/IR/Type.h"

#include <ast/types.h>

struct Generator;

class TypeToLLGenerator {
    Generator& gen;

public: 
    explicit TypeToLLGenerator(Generator& g) : gen(g) {}

    llvm::Type* codegenTy(const Ty& ty);
    llvm::Type* codegenRetTy(const RetTy& ty);

private: 
    llvm::Type* createSignedInt(const Sint si);
    llvm::Type* createUnsignedInt(const Uint ui);
};