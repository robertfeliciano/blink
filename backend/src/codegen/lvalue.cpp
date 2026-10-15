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

            else if constexpr (std::is_same_v<T, EInterfaceProj>) {
                return getInterfaceFieldPtr(node);
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

    llvm::Type* llArrayTy = gen.codegenType(gen.getExpTy(*e.collection));

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

    unsigned idx = gen.classFieldIndex(cd, e.field);

    llvm::StructType* structTy = llvm::cast<llvm::StructType>(gen.codegenType(objTy));
    return gen.builder->CreateStructGEP(structTy, objPtr, idx, "fieldptr");
}

Value* LValueCreator::getInterfaceFieldPtr(const EInterfaceProj& e) {
    const auto& decl = gen.interfaceDecl(*e.receiver);
    if (e.slot >= decl.fields.size())
        throw std::runtime_error("Interface field slot is out of range");
    if (gen.typeGen.codegenValueTy(e.ty) != gen.typeGen.codegenValueTy(decl.fields[e.slot].ftyp))
        throw std::runtime_error("Interface field projection ABI mismatch");
    Value* receiver = gen.codegenExp(*e.receiver);
    Value* object   = gen.interfaceObject(receiver);
    Value* table    = gen.builder->CreateExtractValue(receiver, {1}, "interface_table");
    Value* entry    = gen.interfaceTableEntry(table, decl, 1, e.slot);
    auto* offsetTy  = gen.mod->getDataLayout().getIntPtrType(*gen.ctxt);
    Value* offset   = gen.builder->CreateLoad(offsetTy, entry, "interface_field_offset");
    return gen.builder->CreateInBoundsGEP(gen.builder->getInt8Ty(), object, offset, "interface_field_ptr");
}
