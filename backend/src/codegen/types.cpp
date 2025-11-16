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

inline llvm::Type* TypeToLLGenerator::codegenTy(const Ty& ty) {
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
            switch(*ty.float_ty) {
                case FloatTy::Tf32: 
                    return llvm::Type::getFloatTy(*gen.ctxt);
                case FloatTy::Tf64:
                    return llvm::Type::getDoubleTy(*gen.ctxt);
            }
        default: 
            throw std::runtime_error("Other type not supported yet");
    }
}

inline llvm::Type* TypeToLLGenerator::codegenRetTy(const RetTy& rty) {
    switch (rty.tag) {
        case RetTyTag::RetVoid:
            return llvm::Type::getVoidTy(*gen.ctxt);
        case RetTyTag::RetVal:
            return codegenTy(*rty.val);
    }
}