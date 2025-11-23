#pragma once

#include <llvm/IR/Constants.h>
#include <llvm/IR/Type.h>

#include <bridge/types.h>

struct Generator;

class TypeToLLGenerator {
    Generator& gen;

public: 
    explicit TypeToLLGenerator(Generator& g) : gen(g) {}

    llvm::Type* codegenTy(const Ty& ty);
    llvm::Type* codegenRetTy(const RetTy& ty);
    llvm::Type* codegenRefTy(const RefTy& rt);
private: 
    llvm::Type* createSignedInt(const Sint si);
    llvm::Type* createUnsignedInt(const Uint ui);
    llvm::Type* getStaticArrayType(const RefTy& rt);
    llvm::Type* getClassType(const std::string& cname);
    llvm::Type* getFunctionPointerType(const RefTy& rt);
};