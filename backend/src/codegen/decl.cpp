#include <codegen/generator.h>
#include <codegen/decl.h>

void DeclToLLVisitor::operator()(const Node<GDecl>& g) {
    throw std::runtime_error("global variable declarations not allowed yet!");
}

void DeclToLLVisitor::operator()(const Node<FDecl>& fnode) {
    const FDecl& f = fnode.elt;
    llvm::Function* llFun = gen.mod->getFunction(f.fname);
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(*gen.ctxt, f.fname+"_entry", llFun);

    gen.builder->SetInsertPoint(entryBlock);

    gen.varEnv.clear();
    for (auto& arg : llFun->args()) {
        int argNum = arg.getArgNo();

        std::string argName = f.args[argNum].second;
        llvm::Type* argType = llFun->getFunctionType()->getParamType(argNum);
        gen.varEnv[argName] = gen.builder->CreateAlloca(argType, nullptr, llvm::Twine(argName));

        gen.builder->CreateStore(&arg, gen.varEnv[argName]);
    }

    llvm::Value* retVal;
    for (auto& stmtNode : f.body) {
        retVal = gen.codegenStmt(stmtNode.elt);
    }
    if (llFun->getReturnType()->isVoidTy()) {
        if (!llvm::dyn_cast<llvm::ReturnInst>(retVal)) {
            // if the return type is void but the last statement in the body is not 'return;' we create the RetVoid now
            gen.builder->CreateRetVoid();
        }
    }
    // all other cases have been handled in the statement codegen
  
    llvm::verifyFunction(*llFun);
}

void DeclToLLVisitor::codegenFunctionProto(const FDecl& f) {
    std::vector<llvm::Type*> argTys;
    for (auto& arg : f.args) {
        argTys.push_back(gen.codegenType(arg.first));
    }
    llvm::Type* retTy = gen.codegenRetType(f.rtyp);
    llvm::FunctionType* ftyp = llvm::FunctionType::get(retTy, argTys, false);

    llvm::Function* fn = llvm::Function::Create(ftyp, llvm::Function::ExternalLinkage, f.fname, gen.mod.get());

    unsigned idx = 0;
    for (auto& arg : fn->args()) {
        arg.setName(f.args[idx].second);
        idx++;
    }
}

void DeclToLLVisitor::codegenFunctionProtos(const Program& p) {
    for (auto& decl : p) {
        if (std::holds_alternative<Node<FDecl>>(decl.val)) {
            codegenFunctionProto(std::get<Node<FDecl>>(decl.val).elt);
        }
    }
}