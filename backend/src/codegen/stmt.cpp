#include <codegen/generator.h>
#include <codegen/stmt.h>

// llvm::Value* operator()(const Assn& assn) const {
//     llvm::Function* func = gen.builder->GetInsertBlock()->getParent();
//     auto rhs = gen.codegenExp(assn.rhs->elt);
//     if (const auto s = std::get_if<EVar>(&assn.lhs->elt.val)){
//         auto idVal = gen.codegenExp();

//     } else {

//     }
// }

llvm::Value* StmtToLLVisitor::operator()(const Ret& ret) {
    if (ret.value.has_value()) {
        llvm::Value* retVal = gen.codegenExp(ret.value.value()->elt);
        return gen.builder->CreateRet(retVal);
    } else {
        return gen.builder->CreateRetVoid();
    }
}

llvm::Value* StmtToLLVisitor::operator()(const Assn& s) {
    throw std::runtime_error("StmtToLLVisitor::operator()(Assn) not supported yet");
}

llvm::Value* StmtToLLVisitor::operator()(const VDecl& s) {
    if (s.init == nullptr) {
        throw std::runtime_error("Null initialization for VDecl... How?");
    }
    llvm::Value* init = gen.codegenExp(s.init->elt);
    llvm::Function* parent = gen.builder->GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmp(
        &(parent->getEntryBlock()),
        parent->getEntryBlock().begin()
    );
    llvm::AllocaInst* var = tmp.CreateAlloca(init->getType(), nullptr,
                        llvm::Twine(s.id));
    gen.varEnv[s.id] = var;
    gen.builder->CreateStore(init, var);
    return init;
}

llvm::Value* StmtToLLVisitor::operator()(const SCall& s) {
    throw std::runtime_error("StmtToLLVisitor::operator()(SCall) not supported yet");
}

llvm::Value* StmtToLLVisitor::operator()(const If& s) {
    throw std::runtime_error("StmtToLLVisitor::operator()(If) not supported yet");
}

llvm::Value* StmtToLLVisitor::operator()(const While& s) {
    throw std::runtime_error("StmtToLLVisitor::operator()(While) not supported yet");
}

llvm::Value* StmtToLLVisitor::operator()(const Break& s) {
    throw std::runtime_error("StmtToLLVisitor::operator()(Break) not supported yet");
}

llvm::Value* StmtToLLVisitor::operator()(const Continue& s) {
    throw std::runtime_error("StmtToLLVisitor::operator()(Continue) not supported yet");
}
