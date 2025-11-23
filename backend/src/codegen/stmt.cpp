#include <codegen/generator.h>
#include <codegen/stmt.h>

llvm::Value* StmtToLLVisitor::codegenLValue(const Exp& e) {
    return std::visit([&](auto const& node) -> llvm::Value* {

        using T = std::decay_t<decltype(node)>;

        if constexpr (std::is_same_v<T, EId>) {
            auto it = gen.varEnv.find(node.id);
            if (it == gen.varEnv.end()) {
                throw std::runtime_error("Unknown variable (lvalue): " + node.id);
            }
            return it->second; // allocaInst*
        }

        else if constexpr (std::is_same_v<T, EProj>) {
            return gen.getStructFieldPtr(node);
        }

        else if constexpr (std::is_same_v<T, EIndex>) {
            llvm::Value* arrPtr = codegenLValue(*node.collection);
            llvm::Value* idx    = gen.codegenExp(*node.index);
        
            const Ty& collTy = gen.getTyFromExp(*node.collection);
        
            llvm::Type* llArrayTy = gen.codegenType(collTy);
            if (!llvm::isa<llvm::ArrayType>(llArrayTy)) {
                throw std::runtime_error("Indexing into non-array type in lvalue");
            }
        
            llvm::Value* zero = llvm::ConstantInt::get(
                llvm::Type::getInt32Ty(*gen.ctxt), 0);
            
            return gen.builder->CreateGEP(
                llArrayTy,
                arrPtr,
                { zero, idx },
                "idx_lvalue"
            );
        }
        

        else {
            throw std::runtime_error("Expression is not assignable");
        }

    }, e.val);
}


Value* StmtToLLVisitor::operator()(const Assn& s) {
    llvm::Value* lhsPtr = codegenLValue(*s.lhs);
    if (!lhsPtr)
        throw std::runtime_error("Assignment to invalid lvalue");
    

    llvm::Value* rhsVal = gen.codegenExp(*s.rhs);
    if (!rhsVal)
        throw std::runtime_error("Assignment RHS produced null value");

    gen.builder->CreateStore(rhsVal, lhsPtr);

    return rhsVal;
    
}

Value* StmtToLLVisitor::operator()(const VDecl& s) {
    if (s.init == nullptr) {
        throw std::runtime_error("Null initialization for VDecl... How?");
    }
    Value* init = gen.codegenExp(*s.init);
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

Value* StmtToLLVisitor::operator()(const Ret& ret) {
    if (ret.value.has_value()) {
        Value* retVal = gen.codegenExp(*ret.value.value());
        return gen.builder->CreateRet(retVal);
    } else {
        return gen.builder->CreateRetVoid();
    }
}

Value* StmtToLLVisitor::operator()(const SCall& s) {
    throw std::runtime_error("SCall not implemented yet.");
    // llvm::Function* callee = gen.mod->getFunction(*s.callee);
}

Value* StmtToLLVisitor::operator()(const If& s) {
    Value* cond = gen.codegenExp(*s.cond);
    if (cond == nullptr) {
        throw std::runtime_error("null if condition! what gives?");
    }
    llvm::Function* parent = gen.builder->GetInsertBlock()->getParent();

    llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*gen.ctxt, "then", parent);
    llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(*gen.ctxt, "else", parent);
    llvm::BasicBlock* finallyBlock = llvm::BasicBlock::Create(*gen.ctxt, "finally", parent);

    gen.builder->CreateCondBr(cond, thenBlock, elseBlock);

    gen.builder->SetInsertPoint(thenBlock);

    for (auto& stmt : s.then_branch) {
        gen.codegenStmt(*stmt);
    }

    thenBlock = gen.builder->GetInsertBlock();
    gen.builder->CreateBr(finallyBlock);
    
    parent->getBasicBlockList().push_back(elseBlock);
    gen.builder->SetInsertPoint(elseBlock);

    for (auto& stmt : s.else_branch) {
        gen.codegenStmt(*stmt);
    }

    elseBlock = gen.builder->GetInsertBlock();
    gen.builder->CreateBr(finallyBlock);
    
    parent->getBasicBlockList().push_back(finallyBlock);
    gen.builder->SetInsertPoint(finallyBlock);

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*gen.ctxt));
}

Value* StmtToLLVisitor::operator()(const While& s) {
    Value* cond = gen.codegenExp(*s.cond);
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
        gen.codegenStmt(*stmt);
    }
    
    loop = gen.builder->GetInsertBlock();
    gen.builder->CreateCondBr(cond, loop, loopEnd);

    parent->getBasicBlockList().push_back(loopEnd);
    gen.builder->SetInsertPoint(loopEnd);
    
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*gen.ctxt));
}

Value* StmtToLLVisitor::operator()(const Break& s) {
    throw std::runtime_error("StmtToLLVisitor::operator()(Break) not supported yet");
}

Value* StmtToLLVisitor::operator()(const Continue& s) {
    throw std::runtime_error("StmtToLLVisitor::operator()(Continue) not supported yet");
}