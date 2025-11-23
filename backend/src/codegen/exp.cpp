#include <codegen/generator.h>
#include <codegen/exp.h>
#include <cstdio>

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

static llvm::APInt makeAPInt128(unsigned numBits, unsigned __int128 v) {
    uint64_t words[2] = {
        (uint64_t)v,
        (uint64_t)(v >> 64)
    };
    return llvm::APInt(numBits, 2, words);
}

static llvm::APInt makeAPSInt128(unsigned numBits, __int128 v) {
    uint64_t words[2] = {
        (uint64_t)v,
        (uint64_t)(v >> 64)
    };
    return llvm::APInt(numBits, 2, words);
}


Value* ExpToLLVisitor::operator()(const EInt& e) {
    short int_sz = getIntSize(e);
    if (int_sz < 128) {
        if (e.int_ty->tag == IntTyTag::Unsigned)
            return llvm::ConstantInt::get(*gen.ctxt, llvm::APInt(int_sz, e.u));
        else
            return llvm::ConstantInt::get(*gen.ctxt, llvm::APInt(int_sz, e.s));
    } else {
        llvm::APInt ap = (e.int_ty->tag == IntTyTag::Unsigned)
        ? makeAPInt128(int_sz, e.u)
        : makeAPSInt128(int_sz, e.s);

        return llvm::ConstantInt::get(*gen.ctxt, ap);
    }
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

    auto isUnsignedIntTy = [&](const Ty& ty) -> bool {
        if (ty.tag != TyTag::TInt)
            return false;
        return ty.int_ty->tag == IntTyTag::Unsigned;
    };

    bool lhsUnsigned = isUnsignedIntTy(getExpTy(*e.left));
    bool rhsUnsigned = isUnsignedIntTy(getExpTy(*e.right));
    
    bool bothUnsigned = lhsUnsigned && rhsUnsigned;

    auto addWrapFlags = [&](Value* inst) {
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


Value* ExpToLLVisitor::operator()(const EUop& e) {
    throw new std::runtime_error("not supported yet");
}
Value* ExpToLLVisitor::operator()(const EStr& e) { 
    throw new std::runtime_error("not supported yet");
}
Value* ExpToLLVisitor::operator()(const ECall& e) { 
    throw new std::runtime_error("not supported yet");
}
Value* ExpToLLVisitor::operator()(const EIndex& e) { 
    throw new std::runtime_error("not supported yet");
}
Value* ExpToLLVisitor::operator()(const EArray& e) { 
    throw new std::runtime_error("not supported yet");
}
Value* ExpToLLVisitor::operator()(const ECast& e) { 
    throw new std::runtime_error("not supported yet");
}
Value* ExpToLLVisitor::operator()(const EProj& e) { 
    llvm::Value* fieldPtr = gen.getStructFieldPtr(e);

    llvm::Type* fieldTy = gen.codegenType(e.ty);

    return gen.builder->CreateLoad(fieldTy, fieldPtr, "fieldVal");
}
Value* ExpToLLVisitor::operator()(const EObjInit& e) {
    auto it = gen.structTypes.find(e.id);
    if (it == gen.structTypes.end()) {
        throw std::runtime_error("Unknown class: " + e.id);
    }

    llvm::StructType* structTy = it->second;
    llvm::PointerType* structPtrTy = llvm::PointerType::getUnqual(structTy);

    const llvm::DataLayout& dl = gen.mod->getDataLayout();
    uint64_t size = dl.getTypeAllocSize(structTy);

    llvm::Type* i64Ty = llvm::Type::getInt64Ty(*gen.ctxt);
    llvm::Value* sizeVal = llvm::ConstantInt::get(i64Ty, size);

    // TODO add malloc to blink's stdlib
    llvm::Function* mallocFn = gen.mod->getFunction("malloc");
    if (!mallocFn) {
        llvm::FunctionType* mallocFTy =
            llvm::FunctionType::get(
                llvm::Type::getInt8PtrTy(*gen.ctxt),
                { i64Ty },
                false
            );

        mallocFn = llvm::Function::Create(
            mallocFTy,
            llvm::Function::ExternalLinkage,
            "malloc",
            gen.mod.get()
        );
    }

    llvm::Value* rawPtr = gen.builder->CreateCall(mallocFn, { sizeVal }, "rawmem");

    llvm::Value* objPtr = gen.builder->CreateBitCast(rawPtr, structPtrTy, "obj");

    std::unordered_map<std::string, llvm::Value*> userInitMap;

    for (auto& [fname, fexp] : e.fields) {
        userInitMap[fname] = gen.codegenExp(*fexp);
    }

    const CDecl& classDecl = *gen.classEnv.at(e.id);

    for (unsigned idx = 0; idx < classDecl.fields.size(); ++idx) {
        const Field& fd = classDecl.fields[idx];
        llvm::Type* fty = gen.codegenType(fd.ftyp);

        for (auto& stmtPtr : fd.prelude) {
            gen.codegenStmt(*stmtPtr); 
        }

        llvm::Value* storedVal = nullptr;

        if (userInitMap.contains(fd.fieldName)) {
            storedVal = userInitMap[fd.fieldName];
        } else if (fd.init) {
            storedVal = gen.codegenExp(*fd.init);
        } else {
            storedVal = llvm::Constant::getNullValue(fty);
        }

        llvm::Value* fieldPtr =
            gen.builder->CreateStructGEP(structTy, objPtr, idx, fd.fieldName + "_ptr");

        gen.builder->CreateStore(storedVal, fieldPtr);
    }

    return objPtr;
}

llvm::Value* ExpToLLVisitor::getStructFieldPtr(const EProj& e) {
    llvm::Value* objPtr = gen.codegenExp(*e.obj);

    const Ty& objTy = gen.getTyFromExp(*e.obj);

    if (objTy.tag != TyTag::TRef || objTy.ref_ty->tag != RefTyTag::RClass) 
        throw std::runtime_error("Expected reference type (class)");

    const CDecl& cd = *gen.classEnv.at(objTy.ref_ty->cname);

    unsigned idx = 0;
    bool found = false;
    for (; idx < cd.fields.size(); ++idx) {
        if (cd.fields[idx].fieldName == e.field) {
            found = true;
            break;
        }
    }

    if (!found) throw std::runtime_error("Unknown field: " + e.field);


    llvm::StructType* structTy = llvm::cast<llvm::StructType>(gen.codegenType(objTy));

    return gen.builder->CreateStructGEP(structTy, objPtr, idx, "fieldptr");
}