#include <codegen/decl.h>
#include <codegen/generator.h>
#include <llvm/IR/DataLayout.h>

Generator::Generator()
    : ctxt(std::make_unique<llvm::LLVMContext>()), builder(std::make_unique<llvm::IRBuilder<>>(*ctxt)),
      mod(std::make_unique<llvm::Module>("Module", *ctxt)), expVisitor(*this), stmtVisitor(*this), typeGen(*this),
      declVisitor(*this), lvalueCreator(*this) {}

std::optional<VariableStorage> Generator::findVariableStorage(const std::string& id) const {
    if (auto it = varEnv.find(id); it != varEnv.end()) {
        return VariableStorage{it->second, it->second->getAllocatedType()};
    }
    if (auto it = globalEnv.find(id); it != globalEnv.end()) {
        return VariableStorage{it->second, it->second->getValueType()};
    }
    return std::nullopt;
}

void Generator::codegenProgram(const Program& p) {
    for (const auto& decl : p.classes) {
        codegenCDecl(decl);
    }
    for (const auto& interface : p.interfaces)
        interfaceEnv.emplace(interface.iname, &interface);
    codegenStdlib();
    codegenFunctionProtos(p);
    declVisitor.codegenGlobals(p.globals);
    codegenInterfaceTables(p);
    for (const auto& decl : p.functions) {
        codegenFDecl(decl);
    }
    optimize(p.optimizationLevel);
}

llvm::FunctionType* Generator::codegenInterfaceMethodType(const Proto& proto) {
    std::vector<llvm::Type*> params{llvm::PointerType::getUnqual(*ctxt)};
    for (const auto& param : proto.args)
        params.push_back(typeGen.codegenValueTy(param));
    return llvm::FunctionType::get(typeGen.codegenValueRetTy(proto.frtyp), params, false);
}

unsigned Generator::classFieldIndex(const CDecl& decl, const std::string& field) const {
    for (unsigned i = 0; i < decl.fields.size(); ++i)
        if (decl.fields[i].fieldName == field)
            return i;
    throw std::runtime_error("Unknown field: " + decl.cname + "." + field);
}

const InterfaceDecl& Generator::interfaceDecl(const Exp& receiver) const {
    const Ty& ty = ExpToLLVisitor::getExpTy(receiver);
    if (ty.tag != TyTag::TRef || ty.ref_ty->tag != RefTyTag::RInterface)
        throw std::runtime_error("Interface receiver does not have interface type");
    return *interfaceEnv.at(ty.ref_ty->cname);
}

llvm::StructType* Generator::interfaceTableType(const InterfaceDecl& decl) {
    auto* methods = llvm::ArrayType::get(llvm::PointerType::getUnqual(*ctxt), decl.protos.size());
    auto* offsets = llvm::ArrayType::get(mod->getDataLayout().getIntPtrType(*ctxt), decl.fields.size());
    return llvm::StructType::get(*ctxt, {methods, offsets});
}

llvm::Value* Generator::interfaceTableEntry(llvm::Value* table, const InterfaceDecl& decl, unsigned section,
                                            unsigned slot) {
    if (section > 1 || slot >= (section == 0 ? decl.protos.size() : decl.fields.size()))
        throw std::runtime_error("Interface table slot is out of range");
    return builder->CreateInBoundsGEP(interfaceTableType(decl), table,
                                      {builder->getInt32(0), builder->getInt32(section), builder->getInt32(slot)},
                                      "interface_slot");
}

void Generator::codegenInterfaceTables(const Program& p) {
    for (const auto& cdecl : p.classes) {
        for (const auto& impl : cdecl.impls) {
            const auto& decl   = *interfaceEnv.at(impl.iname);
            const auto& protos = decl.protos;
            if (protos.size() != impl.methods.size())
                throw std::runtime_error("Interface method table has the wrong number of methods");
            std::vector<llvm::Constant*> methods;
            for (size_t slot = 0; slot < protos.size(); ++slot) {
                llvm::Function* method = mod->getFunction(impl.methods[slot]);
                if (!method)
                    throw std::runtime_error("Unknown interface implementation method: " + impl.methods[slot]);
                auto* expectedType = codegenInterfaceMethodType(protos[slot]);
                if (method->getFunctionType() != expectedType)
                    throw std::runtime_error("Interface implementation ABI mismatch: " + impl.methods[slot]);
                methods.push_back(method);
            }
            std::vector<llvm::Constant*> offsets;
            auto*                        structTy = structTypes.at(cdecl.cname);
            const auto*                  layout   = mod->getDataLayout().getStructLayout(structTy);
            auto*                        offsetTy = mod->getDataLayout().getIntPtrType(*ctxt);
            for (const auto& field : decl.fields) {
                unsigned index = classFieldIndex(cdecl, field.fieldName);
                if (typeGen.codegenValueTy(cdecl.fields[index].ftyp) != typeGen.codegenValueTy(field.ftyp))
                    throw std::runtime_error("Interface field ABI mismatch: " + cdecl.cname + "." + field.fieldName);
                offsets.push_back(llvm::ConstantInt::get(offsetTy, layout->getElementOffset(index)));
            }
            auto* tableType = interfaceTableType(decl);
            auto* methodArray =
                llvm::ConstantArray::get(llvm::cast<llvm::ArrayType>(tableType->getElementType(0)), methods);
            auto* offsetArray =
                llvm::ConstantArray::get(llvm::cast<llvm::ArrayType>(tableType->getElementType(1)), offsets);
            auto* table = new llvm::GlobalVariable(*mod, tableType, true, llvm::GlobalValue::PrivateLinkage,
                                                   llvm::ConstantStruct::get(tableType, {methodArray, offsetArray}),
                                                   "blink.interface.table." + std::to_string(interfaceTables.size()));
            interfaceTables.emplace(std::make_pair(cdecl.cname, impl.iname), table);
        }
    }
}

llvm::Value* Generator::interfaceObject(llvm::Value* value) {
    return builder->CreateExtractValue(value, {0}, "interface_receiver");
}

llvm::Value* Generator::codegenInterfaceCall(const Exp& receiver, unsigned slot,
                                             const std::vector<std::unique_ptr<Exp>>& args) {
    const auto& decl   = interfaceDecl(receiver);
    const auto& protos = decl.protos;
    if (slot >= protos.size())
        throw std::runtime_error("Interface method slot is out of range");
    const Proto& proto = protos[slot];
    if (args.size() != proto.args.size())
        throw std::runtime_error("Interface method call has the wrong argument count");
    auto*                     pointerTy = llvm::PointerType::getUnqual(*ctxt);
    llvm::Value*              value     = codegenExp(receiver);
    llvm::Value*              object    = interfaceObject(value);
    llvm::Value*              table     = builder->CreateExtractValue(value, {1}, "interface_table");
    llvm::Value*              slotPtr   = interfaceTableEntry(table, decl, 0, slot);
    llvm::Value*              method    = builder->CreateLoad(pointerTy, slotPtr, "interface_method");
    std::vector<llvm::Value*> values{object};
    for (size_t i = 0; i < args.size(); ++i) {
        values.push_back(codegenExp(*args[i]));
    }
    auto* fnType = codegenInterfaceMethodType(proto);
    return builder->CreateCall(fnType, method, values, fnType->getReturnType()->isVoidTy() ? "" : "interface_call");
}

void Generator::codegenStdlib() {
    llvm::FunctionType* exit_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*this->ctxt), {llvm::Type::getInt32Ty(*this->ctxt)}, false);

    llvm::Function* exit_func =
        llvm::Function::Create(exit_type, llvm::Function::ExternalLinkage, "exit", this->mod.get());

    llvm::FunctionType* free_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*this->ctxt), {llvm::Type::getInt8PtrTy(*this->ctxt)}, false);

    llvm::Function* free_func =
        llvm::Function::Create(free_type, llvm::Function::ExternalLinkage, "free", this->mod.get());
}

void Generator::configureTarget() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    auto triple = llvm::sys::getDefaultTargetTriple();
    mod->setTargetTriple(triple);

    std::string         error;
    const llvm::Target* target = llvm::TargetRegistry::lookupTarget(triple, error);
    if (target == nullptr) {
        throw std::runtime_error("Could not configure target " + triple + ": " + error);
    }

    llvm::TargetOptions targetOptions;
    targetMachine.reset(target->createTargetMachine(triple, "generic", "", targetOptions, llvm::Reloc::PIC_));
    if (targetMachine == nullptr) {
        throw std::runtime_error("Could not create target machine for " + triple);
    }
    mod->setDataLayout(targetMachine->createDataLayout());
}

void Generator::optimize(BlinkOptimizationLevel optimizationLevel) {
    llvm::PassBuilder passBuilder(targetMachine.get());

    llvm::LoopAnalysisManager     lam;
    llvm::FunctionAnalysisManager fam;
    llvm::CGSCCAnalysisManager    cgam;
    llvm::ModuleAnalysisManager   mam;

    passBuilder.registerModuleAnalyses(mam);
    passBuilder.registerCGSCCAnalyses(cgam);
    passBuilder.registerFunctionAnalyses(fam);
    passBuilder.registerLoopAnalyses(lam);
    passBuilder.crossRegisterProxies(lam, fam, cgam, mam);

    llvm::OptimizationLevel llvmLevel;
    switch (optimizationLevel) {
        case BlinkOptimizationLevel::O0:
            llvmLevel = llvm::OptimizationLevel::O0;
            break;
        case BlinkOptimizationLevel::O1:
            llvmLevel = llvm::OptimizationLevel::O1;
            break;
        case BlinkOptimizationLevel::O2:
            llvmLevel = llvm::OptimizationLevel::O2;
            break;
        case BlinkOptimizationLevel::O3:
            llvmLevel = llvm::OptimizationLevel::O3;
            break;
    }

    /*
    LLVM's level-specific default pipelines are cumulative: each level
    contains every optimization LLVM considers appropriate up to that level.
    */
    llvm::ModulePassManager mpm = llvmLevel == llvm::OptimizationLevel::O0
                                      ? passBuilder.buildO0DefaultPipeline(llvmLevel)
                                      : passBuilder.buildPerModuleDefaultPipeline(llvmLevel);
    mpm.run(*mod, mam);
}

void Generator::dumpLL(const std::string& filename) {
    std::error_code      EC;
    llvm::raw_fd_ostream outFile(filename, EC);

    if (EC) {
        throw std::runtime_error("Could not open file " + filename + ": " + EC.message());
    }
    mod->print(outFile, nullptr);
}
