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
    // TODO put flag in ocaml side for optimization level
    // optimize();
}

void Generator::codegenStdlib() {
    llvm::FunctionType* printf_type =
        llvm::FunctionType::get(llvm::Type::getInt32Ty(*this->ctxt), {llvm::Type::getInt8PtrTy(*this->ctxt)}, true);

    llvm::Function* printf_func =
        llvm::Function::Create(printf_type, llvm::Function::ExternalLinkage, "printf", this->mod.get());

    llvm::FunctionType* exit_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*this->ctxt), {llvm::Type::getInt32Ty(*this->ctxt)}, false);

    llvm::Function* exit_func =
        llvm::Function::Create(exit_type, llvm::Function::ExternalLinkage, "exit", this->mod.get());
}

void Generator::configureTarget() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    auto triple = llvm::sys::getDefaultTargetTriple();
    mod->setTargetTriple(triple);
}

void Generator::optimize() {
    llvm::PassBuilder passBuilder;

    llvm::LoopAnalysisManager     lam;
    llvm::FunctionAnalysisManager fam;
    llvm::CGSCCAnalysisManager    cgam;
    llvm::ModuleAnalysisManager   mam;

    llvm::TargetLibraryInfoImpl tlii(llvm::Triple(mod->getTargetTriple()));
    fam.registerPass([&] { return llvm::TargetLibraryAnalysis(tlii); });

    passBuilder.registerModuleAnalyses(mam);
    passBuilder.registerCGSCCAnalyses(cgam);
    passBuilder.registerFunctionAnalyses(fam);
    passBuilder.registerLoopAnalyses(lam);
    passBuilder.crossRegisterProxies(lam, fam, cgam, mam);

    llvm::FunctionPassManager fpm;

    fpm.addPass(llvm::VerifierPass());
    fpm.addPass(llvm::PromotePass());
    fpm.addPass(llvm::InstCombinePass());
    fpm.addPass(llvm::ReassociatePass());
    fpm.addPass(llvm::GVNPass());
    fpm.addPass(llvm::SimplifyCFGPass());

    for (auto& func : *mod) {
        if (!func.isDeclaration()) {
            fpm.run(func, fam);
        }
    }
}

void Generator::dumpLL(const std::string& filename) {
    std::error_code      EC;
    llvm::raw_fd_ostream outFile(filename, EC);

    if (EC) {
        throw std::runtime_error("Could not open file " + filename + ": " + EC.message());
    }
    mod->print(outFile, nullptr);
}
