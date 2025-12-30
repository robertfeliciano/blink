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
                llvm_unreachable("Expression is not a valid lvalue.");
            }
        },
        e.val);
}

Value* LValueCreator::getArrayElemPtr(const EIndex& e) {
    Value* arrPtr;

    if (holds_any_of<ECall, EObjInit, EArray>(e.collection->val)) {
        // these are intermediaries which are parents of lvalues
        arrPtr = gen.codegenExp(*e.collection);
    } else {
        arrPtr = codegenLValue(*e.collection);
    }

    Value* idx = gen.codegenExp(*e.index);

    llvm::Type* llArrayTy = gen.codegenType(gen.getExpTy(*e.collection));

    if (!llvm::isa<llvm::ArrayType>(llArrayTy)) {
        throw std::runtime_error("Indexing into non-array type in lvalue");
    }

    Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*gen.ctxt), 0);

    Value* finalPtr = arrPtr;

    if (std::holds_alternative<EId>(e.collection->val)) {
        llvm::Type* loadType = llvm::PointerType::getUnqual(*gen.ctxt);

        finalPtr = gen.builder->CreateLoad(loadType, arrPtr, "arr_ptr_load");
    }

    return gen.builder->CreateInBoundsGEP(llArrayTy, finalPtr, {zero, idx}, "idx_lvalue");
}

Value* LValueCreator::getStructFieldPtr(const EProj& e) {
    Value* objPtr;

    if (holds_any_of<ECall, EObjInit, EArray>(e.obj->val)) {
        // these are intermediaries which are parents of lvalues
        objPtr = gen.codegenExp(*e.obj);
    } else {
        objPtr = codegenLValue(*e.obj);
    }

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
    Value*            finalPtr = objPtr;

    // check if the original expression was an EId (a variable stored as ptr-to-ptr on stack/heap)
    // the EId case is the ONLY case where we need the extra load/dereference.
    if (std::holds_alternative<EId>(e.obj->val)) {
        llvm::Type* loadType = llvm::PointerType::getUnqual(*gen.ctxt);

        finalPtr = gen.builder->CreateLoad(loadType, objPtr, "struct_ptr_load");
    }

    return gen.builder->CreateStructGEP(structTy, finalPtr, idx, "fieldptr");
}