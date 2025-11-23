#pragma once

#include <stdlib.h>
#include <map>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
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

#include <bridge/exp.h>
#include <bridge/stmt.h>
#include <bridge/prog.h>
#include <bridge/decl.h>

#include <codegen/types.h>
#include <codegen/exp.h>
#include <codegen/stmt.h>
#include <codegen/decl.h>
#include <codegen/lvalue.h>

struct Generator {
    std::unique_ptr<llvm::LLVMContext> ctxt;
    std::unique_ptr<llvm::IRBuilder<>> builder;
    std::unique_ptr<llvm::Module> mod;

    std::unordered_map<std::string, llvm::AllocaInst *> varEnv;
    std::unordered_map<std::string, llvm::StructType*> structTypes;
    std::unordered_map<std::string, const CDecl*> classEnv;
    

    ExpToLLVisitor expVisitor;
    StmtToLLVisitor stmtVisitor;
    TypeToLLGenerator typeGen;
    DeclToLLVisitor declVisitor;
    LValueCreator lvalueCreator;

    std::vector<llvm::BasicBlock*> breakTargets;
    std::vector<llvm::BasicBlock*> continueTargets;

    void configureTarget();
    void optimize();
    Generator();

    llvm::Value* codegenExp(const Exp& e) {
        return std::visit(expVisitor, e.val);
    }

    llvm::Value* codegenStmt(const Stmt& s) {
        return std::visit(stmtVisitor, s.val);
    }

    llvm::Type* codegenType(const Ty& ty) {
        return typeGen.codegenTy(ty);
    }

    llvm::Type* codegenRetType(const RetTy& ty) {
        return typeGen.codegenRetTy(ty);
    }

    void codegenFunctionProtos(const Program& p) {
        return declVisitor.codegenFunctionPrototypes(p.functions);
    }

    void codegenFDecl(const FDecl& d) {
        return declVisitor.codegenFDecl(d);
    }

    void codegenCDecl(const CDecl& d) {
        return declVisitor.codegenCDecl(d);
    }

    const Ty& getTyFromExp(const Exp& e) {
        return std::visit([](const auto& node) -> const Ty& {
            return node.ty;
        }, e.val);
    }

    void codegenProgram(const Program& p);
    void dumpLL(const std::string& filename);
};
