#pragma once

#include <stdlib.h>

#include <string>
#include <map>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/TargetLibraryInfo.h"

#include <ast/exp.h>
#include <ast/stmt.h>
#include <ast/prog.h>
#include <codegen/exp.h>
#include <codegen/stmt.h>
#include <codegen/types.h>
#include <codegen/decl.h>

struct Generator { 
    const int RESERVED_FIELDS = 4;
    
    std::unique_ptr<llvm::LLVMContext> ctxt;
    std::unique_ptr<llvm::IRBuilder<>> builder;
    std::unique_ptr<llvm::Module> mod;

    std::map<std::string, llvm::AllocaInst *> varEnv;
    
    void configureTarget();

    void optimize();

    Generator();

    llvm::Value* codegenExp(const Exp& e) {
        return std::visit(ExpToLLVisitor(*this), e.val);
    }

    llvm::Value* codegenStmt(const Stmt& s) {
        return std::visit(StmtToLLVisitor(*this), s.val);
    }

    llvm::Type* codegenType(const Ty& ty) {
        return TypeToLLGenerator(*this).codegenTy(ty);
    }

    llvm::Type* codegenRetType(const RetTy& ty) {
        return TypeToLLGenerator(*this).codegenRetTy(ty);
    }

    void codegenFunctionProtos(const Program& p) {
        return DeclToLLVisitor(*this).codegenFunctionProtos(p);
    }

    void codegenDecl(const Decl& d) {
        return std::visit(DeclToLLVisitor(*this), d.val);
    }

    void codegenProgram(const Program& p);

    void dumpLL(const std::string& filename);
};