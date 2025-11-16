#include <codegen/generator.h>
#include <codegen/decl.h>

void DeclToLLVisitor::codegenFunctionProto(const FDecl& fn) {
    std::vector<llvm::Type*> argTys;
    for (auto& arg : fn.args) {
        argTys.push_back(gen.codegenType(arg.first));
    }
    llvm::Type* retTy = gen.codegenRetType(fn.frtyp);
    llvm::FunctionType* ftyp = llvm::FunctionType::get(retTy, argTys, false);

    llvm::Function* llfn = llvm::Function::Create(ftyp, llvm::Function::ExternalLinkage, fn.fname, gen.mod.get());

    unsigned idx = 0;
    for (auto& arg : llfn->args()) {
        arg.setName(fn.args[idx].second);
        idx++;
    }
}

void DeclToLLVisitor::codegenFunctionPrototypes(const std::vector<FDecl>& fns) {
    for (auto& decl : fns) {
        codegenFunctionProto(decl);
    }
}