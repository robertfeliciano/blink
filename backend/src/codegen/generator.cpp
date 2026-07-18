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
    codegenStdlib();
    codegenFunctionProtos(p);
    for (const auto& decl : p.functions) {
        codegenFDecl(decl);
    }
    optimize(p.optimizationLevel);
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
