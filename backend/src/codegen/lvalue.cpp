#include <codegen/generator.h>
#include <codegen/lvalue.h>
#include <codegen/exp.h>

#include "llvm/IR/Value.h"

Value* LValueCreator::codegenLValue(const Exp& e) {
    return std::visit([&](auto const& node) -> Value* {

        using T = std::decay_t<decltype(node)>;

        if constexpr (std::is_same_v<T, EId>) {
            auto it = gen.varEnv.find(node.id);
            if (it == gen.varEnv.end()) {
                throw std::runtime_error("Unknown variable (lvalue): " + node.id);
            }
            return it->second; // allocaInst*
        }

        else if constexpr (std::is_same_v<T, EProj>) {
            return getStructFieldPtr(node);
        }

        else if constexpr (std::is_same_v<T, EIndex>) {
            return getArrayElemPtr(node);
        }

        else {
            throw std::runtime_error("Expression is not assignable");
        }

    }, e.val);
}

Value* LValueCreator::getArrayElemPtr(const EIndex& e) {
    Value* arrPtr = gen.codegenExp(*e.collection);
    Value* idx = gen.codegenExp(*e.index);

    llvm::Type* llArrayTy = gen.codegenType(gen.getExpTy(*e.collection));

    if (!llvm::isa<llvm::ArrayType>(llArrayTy)) {
        throw std::runtime_error("Indexing into non-array type in lvalue");
    }

    Value* zero = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(*gen.ctxt), 0);
    
    return gen.builder->CreateInBoundsGEP(
        llArrayTy,
        arrPtr,
        { zero, idx },
        "idx_lvalue"
    );
}

Value* LValueCreator::getStructFieldPtr(const EProj& e) {
    Value* objPtr = gen.codegenExp(*e.obj);

    const Ty& objTy = gen.getExpTy(*e.obj);

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