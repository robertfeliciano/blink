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
    llvm::Value* cond = gen.codegenExp(s.cond->elt);
    if (cond == nullptr) {
        throw std::runtime_error("null if condition! what gives?");
    }
    llvm::Function* parent = gen.builder->GetInsertBlock()->getParent();

    llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*gen.ctxt, "then", parent);
    llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(*gen.ctxt, "else", parent);
    llvm::BasicBlock* finallyBlock = llvm::BasicBlock::Create(*gen.ctxt, "finally", parent);

    gen.builder->CreateCondBr(cond, thenBlock, elseBlock);

    gen.builder->SetInsertPoint(thenBlock);

    for (auto& s : s.then_branch) {
        gen.codegenStmt(s->elt);
    }

    thenBlock = gen.builder->GetInsertBlock();
    gen.builder->CreateBr(finallyBlock);
    
    parent->getBasicBlockList().push_back(elseBlock);
    gen.builder->SetInsertPoint(elseBlock);

    for (auto& s : s.else_branch) {
        gen.codegenStmt(s->elt);
    }

    elseBlock = gen.builder->GetInsertBlock();
    gen.builder->CreateBr(finallyBlock);
    
    parent->getBasicBlockList().push_back(finallyBlock);
    gen.builder->SetInsertPoint(finallyBlock);

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*gen.ctxt));
}

llvm::Value* StmtToLLVisitor::operator()(const While& s) {
    llvm::Value* cond = gen.codegenExp(s.cond->elt);
    if (cond == nullptr) {
        throw std::runtime_error("null loop condition! what??");
    }

    llvm::Function* parent = gen.builder->GetInsertBlock()->getParent();

    llvm::BasicBlock* loop = llvm::BasicBlock::Create(*gen.ctxt, "loop");
    llvm::BasicBlock* loopEnd = llvm::BasicBlock::Create(*gen.ctxt, "loop_end");

    gen.builder->CreateCondBr(cond, loop, loopEnd);

    parent->getBasicBlockList().push_back(loop);
    gen.builder->SetInsertPoint(loop);

    for (auto& stmt : s.body) {
        gen.codegenStmt(stmt->elt);
    }
    
    loop = gen.builder->GetInsertBlock();
    gen.builder->CreateCondBr(cond, loop, loopEnd);

    parent->getBasicBlockList().push_back(loopEnd);
    gen.builder->SetInsertPoint(loopEnd);
    
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*gen.ctxt));
}

llvm::Value* StmtToLLVisitor::operator()(const Break& s) {
    throw std::runtime_error("StmtToLLVisitor::operator()(Break) not supported yet");
}

llvm::Value* StmtToLLVisitor::operator()(const Continue& s) {
    throw std::runtime_error("StmtToLLVisitor::operator()(Continue) not supported yet");
}
