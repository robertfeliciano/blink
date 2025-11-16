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

Value* ExpToLLVisitor::operator()(const ECall& e) {

}

