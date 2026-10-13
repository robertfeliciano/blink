#include "llvm/IR/Value.h"

#include <codegen/exp.h>
#include <codegen/generator.h>
#include <codegen/lvalue.h>

#include <util/print.h>

Value* LValueCreator::codegenLValue(const Exp& e) {
    return std::visit(
        [&](auto const& node) -> Value* {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, EId>) {
                auto storage = gen.findVariableStorage(node.id);
                if (!storage) {
                    throw std::runtime_error("Unknown variable (lvalue): " + node.id);
                }
                return storage->address;
            }

            else if constexpr (std::is_same_v<T, EProj>) {
                return getStructFieldPtr(node);
            }

            else if constexpr (std::is_same_v<T, EIndex>) {
                return getArrayElemPtr(node);
            }

            else {
                llvm_unreachable("Expression is not a valid lvalue.");
            }
        },
        e.val);
}

Value* LValueCreator::getArrayElemPtr(const EIndex& e) {
    Value* arrPtr = gen.codegenExp(*e.collection);

    Value* idx = gen.codegenExp(*e.index);

    const Ty& arrayTy = gen.getExpTy(*e.collection);
    if (arrayTy.tag == TyTag::TRef && arrayTy.ref_ty->tag == RefTyTag::RDArray) {
        llvm::Type* elemTy = gen.codegenValueType(*arrayTy.ref_ty->inner);
        return gen.builder->CreateInBoundsGEP(elemTy, arrPtr, idx, "idx_lvalue");
    }

    llvm::Type* llArrayTy = gen.codegenType(arrayTy);

    if (!llvm::isa<llvm::ArrayType>(llArrayTy)) {
        throw std::runtime_error("Indexing into non-array type in lvalue");
    }

    Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*gen.ctxt), 0);

    return gen.builder->CreateInBoundsGEP(llArrayTy, arrPtr, {zero, idx}, "idx_lvalue");
}

Value* LValueCreator::getStructFieldPtr(const EProj& e) {
    Value* objPtr = gen.codegenExp(*e.obj);

    const Ty& objTy = gen.getExpTy(*e.obj);

    if (objTy.tag != TyTag::TRef || objTy.ref_ty->tag != RefTyTag::RClass)
        throw std::runtime_error("Expected reference type (class). Received " + tyToString(objTy));

    const CDecl& cd = *gen.classEnv.at(objTy.ref_ty->cname);

    unsigned idx   = 0;
    bool     found = false;
    for (; idx < cd.fields.size(); ++idx) {
        if (cd.fields[idx].fieldName == e.field) {
            found = true;
            break;
        }
    }

    if (!found)
        throw std::runtime_error("Unknown field: " + e.field);

    llvm::StructType* structTy = llvm::cast<llvm::StructType>(gen.codegenType(objTy));
    return gen.builder->CreateStructGEP(structTy, objPtr, idx, "fieldptr");
}
