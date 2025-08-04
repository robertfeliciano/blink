#include "codegen/generator.h"
#include "codegen/exp.h"

llvm::Value* ExpToLLVisitor::operator()(const EInt& e) {
    return llvm::ConstantInt::get(*gen.ctxt, llvm::APInt(32, e.value));
}

llvm::Value* ExpToLLVisitor::operator()(const EBool& e) {
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(*gen.ctxt), e.value);
}

llvm::Value* ExpToLLVisitor::operator()(const EVar& e) {
    auto it = gen.varEnv.find(e.id);
    if (it == gen.varEnv.end()) {
        throw std::runtime_error("Unknown variable: " + e.id);
    }
    return gen.builder->CreateLoad(it->second->getAllocatedType(), it->second, e.id.c_str());
}

llvm::Value* ExpToLLVisitor::operator()(const EFloat& e) {
    return llvm::ConstantFP::get(*gen.ctxt, llvm::APFloat(e.value));
}

llvm::Value* ExpToLLVisitor::operator()(const EUop& e) {
    llvm::Value* arg = gen.codegenExp(e.arg->elt);
    if (!arg) {
        throw new std::runtime_error("bla");
    }
    
    switch (e.op) {
        case UnOp::Neg:
            return gen.builder->CreateNeg(arg, "negtmp");
        case UnOp::Not:
            return gen.builder->CreateNot(arg, "nottmp");
        default:
            throw std::runtime_error("Unknown unary operator");
    }
}

llvm::Value* ExpToLLVisitor::operator()(const EBop& e) {
    llvm::Value* lhs = gen.codegenExp(e.left->elt);
    llvm::Value* rhs = gen.codegenExp(e.right->elt);
    if (!lhs || !rhs) {
        throw new std::runtime_error("bleh");
    }

    switch (e.op) {
        case BinOp::Add:
            return gen.builder->CreateAdd(lhs, rhs, "addtmp");
        case BinOp::Sub:
            return gen.builder->CreateSub(lhs, rhs, "subtmp");
        case BinOp::Mul:
            return gen.builder->CreateMul(lhs, rhs, "multmp");
        case BinOp::Div:
            return gen.builder->CreateSDiv(lhs, rhs, "divtmp");
        case BinOp::Mod:
            return gen.builder->CreateSRem(lhs, rhs, "modtmp");
        case BinOp::Eqeq:
            return gen.builder->CreateICmpEQ(lhs, rhs, "eqtmp");
        case BinOp::Neq:
            return gen.builder->CreateICmpNE(lhs, rhs, "neqtmp");
        case BinOp::Lt:
            return gen.builder->CreateICmpSLT(lhs, rhs, "lttmp");
        case BinOp::Lte:
            return gen.builder->CreateICmpSLE(lhs, rhs, "letmp");
        case BinOp::Gt:
            return gen.builder->CreateICmpSGT(lhs, rhs, "gttmp");
        case BinOp::Gte:
            return gen.builder->CreateICmpSGE(lhs, rhs, "getmp");
        case BinOp::And:
            return gen.builder->CreateAnd(lhs, rhs, "andtmp");
        case BinOp::Or:
            return gen.builder->CreateOr(lhs, rhs, "ortmp");
        default:
            throw std::runtime_error("Unknown binary operator");
    }
}

llvm::Value* ExpToLLVisitor::operator()(const EStr& e) {
    throw std::runtime_error("ExpToLLVisitor::operator()(EStr) not supported yet");
}

llvm::Value* ExpToLLVisitor::operator()(const ECall& e) {
    throw std::runtime_error("ExpToLLVisitor::operator()(ECall) not supported yet");
}

llvm::Value* ExpToLLVisitor::operator()(const EIndex& e) {
    throw std::runtime_error("ExpToLLVisitor::operator()(EIndex) not supported yet");
}

llvm::Value* ExpToLLVisitor::operator()(const EArray& e) {
    throw std::runtime_error("ExpToLLVisitor::operator()(EArray) not supported yet");
}

llvm::Value* ExpToLLVisitor::operator()(const ERange& e) {
    throw std::runtime_error("ExpToLLVisitor::operator()(ERange) not supported yet");
}
