#include <codegen/decl.h>
#include <codegen/generator.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>

void DeclToLLVisitor::codegenFunctionProto(const FDecl& fn) {
    std::vector<llvm::Type*> argTys;
    for (auto& arg : fn.args) {
        const Ty& argTy = arg.first;

        if (is_obj_ty(argTy)) {
            argTys.push_back(llvm::PointerType::getUnqual(*gen.ctxt));
        } else {
            llvm::Type* baseTy = gen.codegenType(argTy);
            argTys.push_back(baseTy);
        }
    }

    const RetTy& retTy = fn.frtyp;
    llvm::Type*  llRetTy;

    if (retTy.tag == RetTyTag::RetVal && is_obj_ty(*retTy.val)) {
        llRetTy = llvm::PointerType::getUnqual(*gen.ctxt);
    } else {
        llRetTy = gen.codegenRetType(fn.frtyp);
    }

    llvm::FunctionType* ftyp = llvm::FunctionType::get(llRetTy, argTys, false);

    // bool C_fun = (std::find(p.annotations.begin(), p.annotations.end(), "C") != p.annotations.end());

    llvm::GlobalValue::LinkageTypes linkage = llvm::Function::InternalLinkage;

    if (fn.fname == "main")
        linkage = llvm::Function::ExternalLinkage;

    llvm::Function* llfn = llvm::Function::Create(ftyp, linkage, fn.fname, gen.mod.get());

    unsigned idx = 0;
    for (auto& arg : llfn->args()) {
        arg.setName(fn.args[idx].second);
        idx++;
    }
}

void DeclToLLVisitor::codegenProto(const Proto& p) {
    std::vector<llvm::Type*> argTys;
    for (auto& argTy : p.args) {
        if (is_obj_ty(argTy)) {
            argTys.push_back(llvm::PointerType::getUnqual(*gen.ctxt));
        } else {
            llvm::Type* baseTy = gen.codegenType(argTy);
            argTys.push_back(baseTy);
        }
    }

    const RetTy& retTy = p.frtyp;
    llvm::Type*  llRetTy;

    if (retTy.tag == RetTyTag::RetVal && is_obj_ty(*retTy.val)) {
        llRetTy = llvm::PointerType::getUnqual(*gen.ctxt);
    } else {
        llRetTy = gen.codegenRetType(p.frtyp);
    }

    bool C_fun = (std::find(p.annotations.begin(), p.annotations.end(), "C") != p.annotations.end());

    llvm::GlobalValue::LinkageTypes linkage = llvm::Function::InternalLinkage;

    if (C_fun)
        linkage = llvm::Function::ExternalLinkage;

    llvm::FunctionType* ftyp = llvm::FunctionType::get(llRetTy, argTys, false);

    llvm::Function* llfn = llvm::Function::Create(ftyp, linkage, p.fname, gen.mod.get());
}

void DeclToLLVisitor::codegenFunctionPrototypes(const std::vector<Proto>& ps, const std::vector<FDecl>& fns) {
    for (auto& proto : ps) {
        codegenProto(proto);
    }
    for (auto& decl : fns) {
        codegenFunctionProto(decl);
    }
}

void DeclToLLVisitor::codegenFDecl(const FDecl& f) {
    llvm::Function*   llFun      = gen.mod->getFunction(f.fname);
    llvm::BasicBlock* entryBlock = llvm::BasicBlock::Create(*gen.ctxt, f.fname + "_entry", llFun);

    gen.builder->SetInsertPoint(entryBlock);

    gen.varEnv.clear();
    for (auto& arg : llFun->args()) {
        int argNum = arg.getArgNo();

        std::string argName = f.args[argNum].second;
        llvm::Type* argType = llFun->getFunctionType()->getParamType(argNum);
        gen.varEnv[argName] = gen.builder->CreateAlloca(argType, nullptr, llvm::Twine(argName));

        gen.builder->CreateStore(&arg, gen.varEnv[argName]);
    }

    llvm::Value* retVal;
    for (auto& stmtNode : f.body) {
        retVal = gen.codegenStmt(*stmtNode);
    }
    if (llFun->getReturnType()->isVoidTy()) {
        if (!llvm::dyn_cast<llvm::ReturnInst>(retVal)) {
            gen.builder->CreateRetVoid();
        }
    }

    llvm::verifyFunction(*llFun);
}

void DeclToLLVisitor::codegenCDecl(const CDecl& cd) {
    std::vector<llvm::Type*> llvmFields;
    llvmFields.reserve(cd.fields.size());

    for (auto& fld : cd.fields) {
        if (is_obj_ty(fld.ftyp)) {
            // obj types (structs/arrays) are stored as ptrs
            llvmFields.push_back(llvm::PointerType::getUnqual(*gen.ctxt));
        } else {
            // primitives are stored directly
            llvmFields.push_back(gen.codegenType(fld.ftyp));
        }
    }

    llvm::StructType* st = llvm::StructType::create(*gen.ctxt, cd.cname);

    st->setBody(llvmFields, /*isPacked=*/false);

    gen.structTypes[cd.cname] = st;
    gen.classEnv[cd.cname]    = &cd;
}
