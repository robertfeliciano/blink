#include <codegen/decl.h>
#include <codegen/generator.h>

Generator::Generator()
    : ctxt(std::make_unique<llvm::LLVMContext>()), builder(std::make_unique<llvm::IRBuilder<>>(*ctxt)),
      mod(std::make_unique<llvm::Module>("Module", *ctxt)), expVisitor(*this), stmtVisitor(*this), typeGen(*this),
      declVisitor(*this), lvalueCreator(*this) {}

void Generator::codegenProgram(const Program& p) {
    for (const auto& decl : p.classes) {
        codegenCDecl(decl);
    }
    for (const auto& interface : p.interfaces)
        interfaceEnv.emplace(interface.iname, &interface);
    codegenStdlib();
    codegenFunctionProtos(p);
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

void Generator::codegenInterfaceTables(const Program& p) {
    auto* pointerTy = llvm::PointerType::getUnqual(*ctxt);
    for (const auto& implementation : p.implementations) {
        const auto& protos = interfaceEnv.at(implementation.iname)->protos;
        if (protos.size() != implementation.methods.size())
            throw std::runtime_error("Interface method table has the wrong number of methods");
        std::vector<llvm::Constant*> methods;
        for (size_t slot = 0; slot < protos.size(); ++slot) {
            llvm::Function* method = mod->getFunction(implementation.methods[slot]);
            if (!method)
                throw std::runtime_error("Unknown interface implementation method: " + implementation.methods[slot]);
            auto* expectedType = codegenInterfaceMethodType(protos[slot]);
            if (method->getFunctionType() != expectedType)
                throw std::runtime_error("Interface implementation ABI mismatch: " + implementation.methods[slot]);
            methods.push_back(method);
        }
        auto* tableType = llvm::ArrayType::get(pointerTy, methods.size());
        auto* table = new llvm::GlobalVariable(*mod, tableType, true, llvm::GlobalValue::PrivateLinkage,
                                               llvm::ConstantArray::get(tableType, methods),
                                               "blink.interface.table." + std::to_string(interfaceTables.size()));
        interfaceTables.emplace(std::make_pair(implementation.cname, implementation.iname), table);
    }
}

llvm::Value* Generator::interfaceObject(llvm::Value* value) {
    return builder->CreateExtractValue(value, {0}, "interface_receiver");
}

llvm::Value* Generator::codegenInterfaceCall(const Exp& receiver, unsigned slot,
                                            const std::vector<std::unique_ptr<Exp>>& args) {
    const Ty& receiverTy = getExpTy(receiver);
    if (receiverTy.tag != TyTag::TRef || receiverTy.ref_ty->tag != RefTyTag::RInterface)
        throw std::runtime_error("Interface call receiver does not have interface type");
    const auto& protos = interfaceEnv.at(receiverTy.ref_ty->cname)->protos;
    if (slot >= protos.size())
        throw std::runtime_error("Interface method slot is out of range");
    const Proto& proto = protos[slot];
    if (args.size() != proto.args.size())
        throw std::runtime_error("Interface method call has the wrong argument count");
    auto* pointerTy = llvm::PointerType::getUnqual(*ctxt);
    llvm::Value* value = codegenExp(receiver);
    llvm::Value* object = interfaceObject(value);
    llvm::Value* table = builder->CreateExtractValue(value, {1}, "interface_table");
    llvm::Value* slotPtr = builder->CreateInBoundsGEP(pointerTy, table, builder->getInt64(slot), "interface_slot");
    llvm::Value* method = builder->CreateLoad(pointerTy, slotPtr, "interface_method");
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

    std::string error;
    const llvm::Target* target = llvm::TargetRegistry::lookupTarget(triple, error);
    if (target == nullptr) {
        throw std::runtime_error("Could not configure target " + triple + ": " + error);
    }

    llvm::TargetOptions targetOptions;
    targetMachine.reset(
        target->createTargetMachine(triple, "generic", "", targetOptions, llvm::Reloc::PIC_));
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
    llvm::ModulePassManager mpm =
        llvmLevel == llvm::OptimizationLevel::O0
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
