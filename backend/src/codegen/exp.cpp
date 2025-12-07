#include <codegen/exp.h>
#include <codegen/generator.h>
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
    uint64_t words[2] = {(uint64_t)v, (uint64_t)(v >> 64)};
    return llvm::APInt(numBits, 2, words);
}

static llvm::APInt makeAPSInt128(unsigned numBits, __int128 v) {
    uint64_t words[2] = {(uint64_t)v, (uint64_t)(v >> 64)};
    return llvm::APInt(numBits, 2, words);
}

Value* ExpToLLVisitor::operator()(const EInt& e) {
    short       int_sz = getIntSize(e);
    llvm::APInt ap;
    bool        isUnsigned = e.int_ty->tag == IntTyTag::Unsigned;

    if (int_sz < 128) {
        ap = llvm::APInt(int_sz, isUnsigned ? e.u : e.s);
    } else {
        ap = isUnsigned ? makeAPInt128(int_sz, e.u) : makeAPSInt128(int_sz, e.s);
    }

    return llvm::ConstantInt::get(*gen.ctxt, ap);
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
        return bothUnsigned ? gen.builder->CreateUDiv(lhs, rhs, "udivtmp")
                            : gen.builder->CreateSDiv(lhs, rhs, "sdivtmp");

    case BinOp::Mod:
        return bothUnsigned ? gen.builder->CreateURem(lhs, rhs, "uremtmp")
                            : gen.builder->CreateSRem(lhs, rhs, "sremtmp");

    case BinOp::Eqeq:
        return gen.builder->CreateICmpEQ(lhs, rhs, "eqtmp");

    case BinOp::Neq:
        return gen.builder->CreateICmpNE(lhs, rhs, "neqtmp");

    case BinOp::Lt:
        return bothUnsigned ? gen.builder->CreateICmpULT(lhs, rhs, "lttmp")
                            : gen.builder->CreateICmpSLT(lhs, rhs, "lttmp");

    case BinOp::Lte:
        return bothUnsigned ? gen.builder->CreateICmpULE(lhs, rhs, "letmp")
                            : gen.builder->CreateICmpSLE(lhs, rhs, "letmp");

    case BinOp::Gt:
        return bothUnsigned ? gen.builder->CreateICmpUGT(lhs, rhs, "gttmp")
                            : gen.builder->CreateICmpSGT(lhs, rhs, "gttmp");

    case BinOp::Gte:
        return bothUnsigned ? gen.builder->CreateICmpUGE(lhs, rhs, "getmp")
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
    std::vector<Value*> args;
    args.reserve(e.args.size());

    for (const auto& arg : e.args) {
        Value* val = gen.codegenExp(*arg);
        if (!val)
            llvm_unreachable("Failed to create LL argument.");

        args.push_back(val);
    }

    if (!std::holds_alternative<EId>(e.callee->val))
        llvm_unreachable("We do not currently support higher-order functions.");

    const EId& idNode = std::get<EId>(e.callee->val);

    llvm::Function* callee = gen.mod->getFunction(idNode.id);

    if (!callee)
        llvm_unreachable("Calling unknown function.");

    return gen.builder->CreateCall(callee, args, "call");
}

Value* ExpToLLVisitor::operator()(const EIndex& e) {
    Value* elemPtr = gen.lvalueCreator.getArrayElemPtr(e);

    llvm::Type* resultTy = gen.codegenType(e.ty);

    if (resultTy->isAggregateType()) {
        // some kind of collection, like an array
        return elemPtr;
    } else {
        // primitive value
        return gen.builder->CreateLoad(resultTy, elemPtr, "idx_load");
    }
}

Value* ExpToLLVisitor::operator()(const EArray& e) {
    llvm::Type* arrTy = gen.codegenType(e.ty);

    Value* arrPtr = gen.builder->CreateAlloca(arrTy, nullptr, "array_lit");
    Value* zero   = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*gen.ctxt), 0);

    llvm::Type* innerTy     = arrTy->getArrayElementType();
    bool        isAggregate = innerTy->isAggregateType();

    llvm::DataLayout dl         = gen.mod->getDataLayout();
    Value*           size       = nullptr;
    unsigned         align      = 0;
    Value*           isVolatile = nullptr;

    if (isAggregate) {
        uint64_t subArraySize = dl.getTypeStoreSize(innerTy);
        size                  = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*gen.ctxt), subArraySize);
        align                 = (unsigned)dl.getABITypeAlign(innerTy).value();
        isVolatile            = llvm::ConstantInt::getFalse(*gen.ctxt);
    }

    for (size_t i = 0; i < e.elements.size(); i++) {
        Value* val = gen.codegenExp(*e.elements[i]);
        Value* idx = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*gen.ctxt), i);

        Value* elemPtr = gen.builder->CreateInBoundsGEP(arrTy, arrPtr, {zero, idx}, "elem_ptr");

        if (isAggregate) {
            Value* dest = gen.builder->CreateBitCast(elemPtr, llvm::Type::getInt8PtrTy(*gen.ctxt), "DestI8Ptr");
            Value* src  = gen.builder->CreateBitCast(val, llvm::Type::getInt8PtrTy(*gen.ctxt), "SrcI8Ptr");

            gen.builder->CreateMemCpy(dest, llvm::MaybeAlign(align), src, llvm::MaybeAlign(align), size, isVolatile);
        } else {
            gen.builder->CreateStore(val, elemPtr);
        }
    }

    return arrPtr;
}

Value* ExpToLLVisitor::operator()(const ECast& e) {
    throw new std::runtime_error("not supported yet");
}

Value* ExpToLLVisitor::operator()(const EProj& e) {
    Value* fieldPtr = gen.lvalueCreator.getStructFieldPtr(e);

    llvm::Type* fieldTy;
    if (is_obj_ty(e.ty)) {
        // load a pointer for obj (struct/array) types
        fieldTy = llvm::PointerType::getUnqual(*gen.ctxt);
    } else {
        fieldTy = gen.codegenType(e.ty);
    }

    return gen.builder->CreateLoad(fieldTy, fieldPtr, "fieldVal");
}

Value* ExpToLLVisitor::operator()(const EObjInit& e) {
    auto it = gen.structTypes.find(e.id);
    if (it == gen.structTypes.end()) {
        throw std::runtime_error("Unknown class: " + e.id);
    }

    llvm::StructType*  structTy    = it->second;
    llvm::PointerType* structPtrTy = llvm::PointerType::getUnqual(structTy);

    const llvm::DataLayout& dl   = gen.mod->getDataLayout();
    uint64_t                size = dl.getTypeAllocSize(structTy);

    llvm::Type* i64Ty   = llvm::Type::getInt64Ty(*gen.ctxt);
    Value*      sizeVal = llvm::ConstantInt::get(i64Ty, size);

    // TODO add malloc to blink's stdlib
    llvm::Function* mallocFn = gen.mod->getFunction("malloc");
    if (!mallocFn) {
        llvm::FunctionType* mallocFTy = llvm::FunctionType::get(llvm::Type::getInt8PtrTy(*gen.ctxt), {i64Ty}, false);

        mallocFn = llvm::Function::Create(mallocFTy, llvm::Function::ExternalLinkage, "malloc", gen.mod.get());
    }

    Value* rawPtr = gen.builder->CreateCall(mallocFn, {sizeVal}, "rawmem");

    Value* objPtr = gen.builder->CreateBitCast(rawPtr, structPtrTy, "obj");

    std::unordered_map<std::string, Value*> userInitMap;

    for (auto& [fname, fexp] : e.fields) {
        userInitMap[fname] = gen.codegenExp(*fexp);
    }

    const CDecl& classDecl = *gen.classEnv.at(e.id);

    for (unsigned idx = 0; idx < classDecl.fields.size(); ++idx) {
        const Field& fd = classDecl.fields[idx];
        
        llvm::Type* storageTy;
        if (is_obj_ty(fd.ftyp)) {
            storageTy = llvm::PointerType::getUnqual(*gen.ctxt);
        } else {
            storageTy = gen.codegenType(fd.ftyp);
        }

        for (auto& stmtPtr : fd.prelude) {
            gen.codegenStmt(*stmtPtr);
        }

        Value* storedVal = nullptr;

        if (userInitMap.contains(fd.fieldName)) {
            storedVal = userInitMap[fd.fieldName];
        } else if (fd.init) {
            storedVal = gen.codegenExp(*fd.init);
        } else {
            storedVal = llvm::Constant::getNullValue(storageTy);
        }

        Value* fieldPtr = gen.builder->CreateStructGEP(structTy, objPtr, idx, fd.fieldName + "_ptr");

        gen.builder->CreateStore(storedVal, fieldPtr);
    }

    return objPtr;
}

Value* ExpToLLVisitor::operator()(const ENull& e) {
    llvm::Type* llty = gen.codegenType(e.ty);

    llvm::PointerType* PtrTy = llvm::PointerType::get(llty, 0); 
    //todo check this works for classes and stuff

    return llvm::ConstantPointerNull::get(PtrTy);
}