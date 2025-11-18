#include <codegen/generator.h>
#include <codegen/exp.h>

short getIntSize(const EInt& e) {
    if (e.int_ty->tag == IntTyTag::Unsigned) {
        switch (e.int_ty->uint) {
            case Uint::Tu8:
                return 8;
            case Uint::Tu16:
                return 16;
            case Uint::Tu32:
                return 32;
            case Uint::Tu64:
                return 64;
            case Uint::Tu128:
                return 128;
        }
    } else {
        switch (e.int_ty->sint) {
            case Sint::Ti8:
                return 8;
            case Sint::Ti16:
                return 16;
            case Sint::Ti32:
                return 32;
            case Sint::Ti64:
                return 64;
            case Sint::Ti128:
                return 128;
        }
    }
}

Value* ExpToLLVisitor::operator()(const EInt& e) {
    short int_sz = getIntSize(e);
    if (e.int_ty->tag == IntTyTag::Unsigned)
        return llvm::ConstantInt::get(*gen.ctxt, llvm::APInt(int_sz, e.u));
    else
        return llvm::ConstantInt::get(*gen.ctxt, llvm::APInt(int_sz, e.s));
}

Value* ExpToLLVisitor::operator()(const EBool& e) {
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(*gen.ctxt), e.value);
}

Value* ExpToLLVisitor::operator()(const EId& e) {
    auto it = gen.varEnv.find(e.id);
    if (it == gen.varEnv.end()) {
        llvm_unreachable("Unknown identifier");
    }
    return gen.builder->CreateLoad(it->second->getAllocatedType(), it->second, e.id.c_str());    
}

Value* ExpToLLVisitor::operator()(const EFloat& e) {
    llvm::APFloat ap(e.value);

    switch (e.float_ty) {
        case FloatTy::Tf32:
            ap.convert(llvm::APFloat::IEEEsingle(), llvm::APFloat::rmNearestTiesToEven, nullptr);
            break;
        case FloatTy::Tf64:
            break;
        default:
            llvm_unreachable("Unknown float type");
    }

    return llvm::ConstantFP::get(*gen.ctxt, ap);
}

Value* ExpToLLVisitor::operator()(const EBop& e) {
    Value* lhs = gen.codegenExp(*e.left);
    Value* rhs = gen.codegenExp(*e.right);
    if (!lhs || !rhs)
        llvm_unreachable("Unknown operands to binary expression");

    inline auto isUnsignedIntTy = [&](const Ty& ty) -> bool {
        if (ty.tag != TyTag::TInt)
            return false;
        return ty.int_ty->tag == IntTyTag::Unsigned;
    };

    bool lhsUnsigned = isUnsignedIntTy(getExpTy(*e.left));
    bool rhsUnsigned = isUnsignedIntTy(getExpTy(*e.right));
    
    bool bothUnsigned = lhsUnsigned && rhsUnsigned;

    inline auto addWrapFlags = [&](Value* inst) {
        auto* op = llvm::cast<llvm::BinaryOperator>(inst);
        if (bothUnsigned)
            op->setHasNoUnsignedWrap(true);
        else
            op->setHasNoSignedWrap(true);
        return inst;
    };

    switch (e.op) {
        case BinOp::Add: {
            auto* inst = gen.builder->CreateAdd(lhs, rhs, "addtmp");
            return addWrapFlags(inst);
        }

        case BinOp::Sub: {
            auto* inst = gen.builder->CreateSub(lhs, rhs, "subtmp");
            return addWrapFlags(inst);
        }

        case BinOp::Mul: {
            auto* inst = gen.builder->CreateMul(lhs, rhs, "multmp");
            return addWrapFlags(inst);
        }

        case BinOp::Div:
            return bothUnsigned
                ? gen.builder->CreateUDiv(lhs, rhs, "udivtmp")
                : gen.builder->CreateSDiv(lhs, rhs, "sdivtmp");

        case BinOp::Mod:
            return bothUnsigned
                ? gen.builder->CreateURem(lhs, rhs, "uremtmp")
                : gen.builder->CreateSRem(lhs, rhs, "sremtmp");

        case BinOp::Eqeq:
            return gen.builder->CreateICmpEQ(lhs, rhs, "eqtmp");

        case BinOp::Neq:
            return gen.builder->CreateICmpNE(lhs, rhs, "neqtmp");

        case BinOp::Lt:
            return bothUnsigned
                ? gen.builder->CreateICmpULT(lhs, rhs, "lttmp")
                : gen.builder->CreateICmpSLT(lhs, rhs, "lttmp");

        case BinOp::Lte:
            return bothUnsigned
                ? gen.builder->CreateICmpULE(lhs, rhs, "letmp")
                : gen.builder->CreateICmpSLE(lhs, rhs, "letmp");

        case BinOp::Gt:
            return bothUnsigned
                ? gen.builder->CreateICmpUGT(lhs, rhs, "gttmp")
                : gen.builder->CreateICmpSGT(lhs, rhs, "gttmp");

        case BinOp::Gte:
            return bothUnsigned
                ? gen.builder->CreateICmpUGE(lhs, rhs, "getmp")
                : gen.builder->CreateICmpSGE(lhs, rhs, "getmp");

        case BinOp::And:
            return gen.builder->CreateAnd(lhs, rhs, "andtmp");

        case BinOp::Or:
            return gen.builder->CreateOr(lhs, rhs, "ortmp");

        default:
            llvm_unreachable("Unsupported binary operator");
    }
}

/*
Value* ExpToLLVisitor::operator()(const EUop& e) {
    Value* argVal = gen.codegenExp(*e.arg);
    if (!argVal)
        llvm_unreachable("null arg in EUop");

    Ty argTy  = getExpTy(*e.arg);
    bool isInt  = argTy.tag == TyTag::TInt;
    bool isBool = argTy.tag == TyTag::TBool;
    bool isFloat= argTy.tag == TyTag::TFloat;

    switch (e.op) {
        case UnOp::Neg: {
            if (isInt) {
                // Integer NEG = 0 - x
                Value* zero = llvm::ConstantInt::get(argVal->getType(), 0);
                auto* inst = gen.builder->CreateSub(zero, argVal, "negtmp");

                // Add NSW/NUW if you want to assume no overflow
                if (auto* bin = dyn_cast<llvm::BinaryOperator>(inst)) {
                    bin->setHasNoSignedWrap(true);
                    bin->setHasNoUnsignedWrap(true);
                }
                return inst;
            }

            if (isFloat) {
                // Float NEG
                auto* inst = gen.builder->CreateFNeg(argVal, "fnegtmp");

                // Optional: fast-math flags
                if (auto* fop = dyn_cast<llvm::FPMathOperator>(inst)) {
                    llvm::FastMathFlags fmf;
                    fmf.setFast();    // or setNoNaNs(), etc.
                    fop->setFastMathFlags(fmf);
                }
                return inst;
            }

            llvm_unreachable("Neg operator on non-numeric type");
        }

        case UnOp::Not: {
            if (isInt) {
                // Bitwise NOT (~x)
                return gen.builder->CreateNot(argVal, "nottmp");
            }

            if (isBool) {
                // Boolean NOT (!x) = xor with true
                return gen.builder->CreateNot(argVal, "boolnottmp");
            }

            llvm_unreachable("Not operator on unsupported type");
        }
    }

    llvm_unreachable("Unknown unary op");
}
*/

