#include <codegen/generator.h>
#include <codegen/stmt.h>

Value* StmtToLLVisitor::operator()(const Assn& s) {
    // typechecker has lready checked, LHS of assn can only be id, field proj, or index
    Value* lhsPtr = gen.lvalueCreator.codegenLValue(*s.lhs);
    if (!lhsPtr)
        throw std::runtime_error("Assignment to invalid lvalue");

    Value* rhsVal = gen.codegenExp(*s.rhs);
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

    llvm::Type*       llTy   = init->getType();
    llvm::Function*   parent = gen.builder->GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmp(&(parent->getEntryBlock()), parent->getEntryBlock().begin());
    llvm::AllocaInst* var = tmp.CreateAlloca(init->getType(), nullptr, llvm::Twine(s.id));
    gen.varEnv[s.id]      = var;
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
    std::vector<Value*> args;
    args.reserve(s.args.size());

    for (const auto& arg : s.args) {
        Value* val = gen.codegenExp(*arg);
        if (!val)
            llvm_unreachable("Failed to create LL argument.");

        args.push_back(val);
    }

    if (!std::holds_alternative<EId>(s.callee->val))
        llvm_unreachable("We do not currently support higher-order functions.");

    const EId& idNode = std::get<EId>(s.callee->val);

    llvm::Function* callee = gen.mod->getFunction(idNode.id);

    if (!callee)
        llvm_unreachable("Calling unknown function.");

    return gen.builder->CreateCall(callee, args);
}

Value* StmtToLLVisitor::operator()(const If& s) {
    Value* condVal = gen.codegenExp(*s.cond);

    if (!condVal)
        throw std::runtime_error("If condition produced null value");

    if (condVal->getType()->isIntegerTy(1) == false) {
        condVal = gen.builder->CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "ifcond");
    }

    llvm::Function* parentFunc = gen.builder->GetInsertBlock()->getParent();

    llvm::BasicBlock* thenBB  = llvm::BasicBlock::Create(*gen.ctxt, "then", nullptr, nullptr);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*gen.ctxt, "ifmerge", nullptr, nullptr);

    llvm::BasicBlock* elseBB = nullptr;
    if (!s.else_branch.empty()) {
        elseBB = llvm::BasicBlock::Create(*gen.ctxt, "else", nullptr, nullptr);
    } else {
        elseBB = mergeBB;
    }

    gen.builder->CreateCondBr(condVal, thenBB, elseBB);

    thenBB->insertInto(parentFunc);

    gen.builder->SetInsertPoint(thenBB);

    for (const auto& stmt : s.then_branch) {
        gen.codegenStmt(*stmt);
    }

    if (gen.builder->GetInsertBlock()->getTerminator() == nullptr) {
        gen.builder->CreateBr(mergeBB);
    }

    if (!s.else_branch.empty()) {
        elseBB->insertInto(parentFunc);
        gen.builder->SetInsertPoint(elseBB);

        for (const auto& stmt : s.else_branch) {
            gen.codegenStmt(*stmt);
        }

        if (gen.builder->GetInsertBlock()->getTerminator() == nullptr) {
            gen.builder->CreateBr(mergeBB);
        }
    }

    mergeBB->insertInto(parentFunc);
    gen.builder->SetInsertPoint(mergeBB);

    return nullptr;
}

Value* StmtToLLVisitor::operator()(const While& s) {
    llvm::Function* parentFunc = gen.builder->GetInsertBlock()->getParent();

    llvm::BasicBlock* condBB  = llvm::BasicBlock::Create(*gen.ctxt, "while_cond", parentFunc);
    llvm::BasicBlock* bodyBB  = llvm::BasicBlock::Create(*gen.ctxt, "while_body", parentFunc);
    llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(*gen.ctxt, "while_after", parentFunc);

    gen.builder->CreateBr(condBB);

    gen.continueTargets.push_back(condBB);

    gen.breakTargets.push_back(afterBB);

    gen.builder->SetInsertPoint(condBB);

    Value* condVal = gen.codegenExp(*s.cond);

    if (!condVal)
        throw std::runtime_error("While condition produced null value");

    if (!condVal->getType()->isIntegerTy(1)) {
        condVal = gen.builder->CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "loopcond");
    }

    gen.builder->CreateCondBr(condVal, bodyBB, afterBB);

    gen.builder->SetInsertPoint(bodyBB);

    for (const auto& stmt : s.body) {
        gen.codegenStmt(*stmt);
    }

    if (gen.builder->GetInsertBlock()->getTerminator() == nullptr) {
        gen.builder->CreateBr(condBB);
    }

    gen.continueTargets.pop_back();
    gen.breakTargets.pop_back();

    gen.builder->SetInsertPoint(afterBB);

    return nullptr;
}

Value* StmtToLLVisitor::operator()(const Break& s) {
    if (gen.breakTargets.empty()) {
        throw std::runtime_error("Break statement outside of a loop.");
    }
    llvm::BasicBlock* breakTo = gen.breakTargets.back();

    gen.builder->CreateBr(breakTo);

    return nullptr;
}

Value* StmtToLLVisitor::operator()(const Continue& s) {
    if (gen.continueTargets.empty()) {
        throw std::runtime_error("Continue statement outside of a loop.");
    }
    llvm::BasicBlock* contTo = gen.continueTargets.back();

    gen.builder->CreateBr(contTo);

    return nullptr;
}