#pragma once

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/OptimizationLevel.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include <bridge/decl.h>
#include <bridge/exp.h>
#include <bridge/prog.h>
#include <bridge/stmt.h>
#include <codegen/decl.h>
#include <codegen/exp.h>
#include <codegen/lvalue.h>
#include <codegen/stmt.h>
#include <codegen/types.h>
#include <map>
#include <stdlib.h>

struct Generator {
    std::unique_ptr<llvm::LLVMContext>   ctxt;
    std::unique_ptr<llvm::IRBuilder<>>   builder;
    std::unique_ptr<llvm::Module>        mod;
    std::unique_ptr<llvm::TargetMachine> targetMachine;

    std::unordered_map<std::string, llvm::AllocaInst*> varEnv;
    std::unordered_map<std::string, llvm::StructType*> structTypes;
    std::unordered_map<std::string, const CDecl*>      classEnv;

    ExpToLLVisitor    expVisitor;
    StmtToLLVisitor   stmtVisitor;
    TypeToLLGenerator typeGen;
    DeclToLLVisitor   declVisitor;
    LValueCreator     lvalueCreator;

    std::vector<llvm::BasicBlock*> breakTargets;
    std::vector<llvm::BasicBlock*> continueTargets;

    void codegenStdlib();
    void configureTarget();
    void optimize(BlinkOptimizationLevel optimizationLevel);
    Generator();

    llvm::Value* codegenExp(const Exp& e) { return std::visit(expVisitor, e.val); }

    llvm::Value* codegenStmt(const Stmt& s) { return std::visit(stmtVisitor, s.val); }

    llvm::Type* codegenType(const Ty& ty) { return typeGen.codegenTy(ty); }

    llvm::Type* codegenRetType(const RetTy& ty) { return typeGen.codegenRetTy(ty); }

    void codegenFunctionProtos(const Program& p) {
        return declVisitor.codegenFunctionPrototypes(p.protos, p.functions);
    }

    void codegenFDecl(const FDecl& d) { return declVisitor.codegenFDecl(d); }

    void codegenCDecl(const CDecl& d) { return declVisitor.codegenCDecl(d); }

    const Ty& getExpTy(const Exp& e) { return expVisitor.getExpTy(e); }

    void codegenProgram(const Program& p);
    void dumpLL(const std::string& filename);
};
