#include <codegen/generator.h>
#include <codegen/types.h>

llvm::Type* TypeToLLGenerator::createSignedInt(const Sint si) {
    switch (si) {
        case Sint::Ti8:
            return llvm::Type::getInt8Ty(*gen.ctxt);
        case Sint::Ti16:
            return llvm::Type::getInt16Ty(*gen.ctxt);
        case Sint::Ti32:
            return llvm::Type::getInt32Ty(*gen.ctxt);
        case Sint::Ti64:
            return llvm::Type::getInt64Ty(*gen.ctxt);
        case Sint::Ti128:
            return llvm::Type::getInt128Ty(*gen.ctxt);
    }
}

llvm::Type* TypeToLLGenerator::createUnsignedInt(const Uint ui) {
    switch (ui) {
        case Uint::Tu8:
            return llvm::Type::getInt8Ty(*gen.ctxt);
        case Uint::Tu16:
            return llvm::Type::getInt16Ty(*gen.ctxt);
        case Uint::Tu32:
            return llvm::Type::getInt32Ty(*gen.ctxt);
        case Uint::Tu64:
            return llvm::Type::getInt64Ty(*gen.ctxt);
        case Uint::Tu128:
            return llvm::Type::getInt128Ty(*gen.ctxt);
    }
}

llvm::Type* TypeToLLGenerator::codegenTy(const Ty& ty) {
    switch (ty.tag) {
        case TyTag::TBool:
            return llvm::Type::getInt1Ty(*gen.ctxt);
        case TyTag::TInt: {
            switch (ty.int_ty->tag) {
                case IntTyTag::Signed:
                    return createSignedInt(ty.int_ty->sint);
                case IntTyTag::Unsigned:
                    return createUnsignedInt(ty.int_ty->uint);
            }
        }
        case TyTag::TFloat:
            switch (*ty.float_ty) {
                case FloatTy::Tf32:
                    return llvm::Type::getFloatTy(*gen.ctxt);
                case FloatTy::Tf64:
                    return llvm::Type::getDoubleTy(*gen.ctxt);
            }
        case TyTag::TRef:
            return codegenRefTy(*ty.ref_ty);
        default:
            throw std::runtime_error("Other type not supported yet");
    }
}

llvm::Type* TypeToLLGenerator::codegenRetTy(const RetTy& rty) {
    switch (rty.tag) {
        case RetTyTag::RetVoid:
            return llvm::Type::getVoidTy(*gen.ctxt);
        case RetTyTag::RetVal:
            return codegenTy(*rty.val);
    }
}

llvm::Type* TypeToLLGenerator::codegenRefTy(const RefTy& rt) {
    switch (rt.tag) {
        case RefTyTag::RString:
            return llvm::Type::getInt8PtrTy(*gen.ctxt);

        case RefTyTag::RArray:
            return getStaticArrayType(rt);

        case RefTyTag::RClass:
            return getClassType(rt.cname);

        case RefTyTag::RFun:
            return getFunctionPointerType(rt);

        case RefTyTag::RPtr: 
            return llvm::PointerType::getUnqual(*gen.ctxt);

        default:
            throw std::runtime_error("Unknown RefTyTag in codegenRefTy");
    }
}

llvm::Type* TypeToLLGenerator::getStaticArrayType(const RefTy& rt) {
    if (!rt.inner)
        throw std::runtime_error("Array type missing inner type");

    llvm::Type* elemTy = codegenTy(*rt.inner);

    if (rt.size < 0)
        throw std::runtime_error("Static array size must be non-negative");

    return llvm::ArrayType::get(elemTy, static_cast<uint64_t>(rt.size));
}

llvm::Type* TypeToLLGenerator::getClassType(const std::string& cname) {
    auto it = gen.classEnv.find(cname);
    if (it == gen.classEnv.end())
        throw std::runtime_error("Unknown class type: " + cname);

    const CDecl& cd = *it->second;

    if (llvm::StructType* existing = gen.structTypes[cname])
        return existing;
    else
        throw new std::runtime_error("Unknown class type: " + cname);
}

llvm::Type* TypeToLLGenerator::getFunctionPointerType(const RefTy& rt) {
    std::vector<llvm::Type*> argTys;
    argTys.reserve(rt.args.size());

    for (auto& a : rt.args)
        argTys.push_back(codegenTy(a));

    llvm::Type* retTy = gen.codegenRetType(rt.ret);

    llvm::FunctionType* fty = llvm::FunctionType::get(retTy, argTys, false);

    return fty->getPointerTo();
}
