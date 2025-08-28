#include <codegen/generator.h>
#include <codegen/decl.h>

Generator::Generator() {
    ctxt = std::make_unique<llvm::LLVMContext>();
    builder = std::make_unique<llvm::IRBuilder<>>(*ctxt);
    mod = std::make_unique<llvm::Module>("Module", *ctxt);
}

void Generator::codegenProgram(const Program& p) {
    codegenFunctionProtos(p);
    for (const auto& decl : p) {
        codegenDecl(decl);
    }
    optimize();
}

void Generator::configureTarget() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    mod->setTargetTriple(targetTriple);
}

void Generator::optimize() {
    llvm::PassBuilder passBuilder;

    llvm::LoopAnalysisManager lam;
    llvm::FunctionAnalysisManager fam;
    llvm::CGSCCAnalysisManager cgam;
    llvm::ModuleAnalysisManager mam;

    passBuilder.registerModuleAnalyses(mam);
    passBuilder.registerCGSCCAnalyses(cgam);
    passBuilder.registerFunctionAnalyses(fam);
    passBuilder.registerLoopAnalyses(lam);
    passBuilder.crossRegisterProxies(lam, fam, cgam, mam);

    llvm::FunctionPassManager fpm;

    fpm.addPass(llvm::InlinerPass());
    fpm.addPass(llvm::PromotePass());
    fpm.addPass(llvm::InstCombinePass());
    fpm.addPass(llvm::ReassociatePass());
    fpm.addPass(llvm::GVNPass());
    fpm.addPass(llvm::SimplifyCFGPass());

    // Run optimization passes on each function in the module
    for (auto &func : *mod) {
        fpm.run(func, fam);
    }
}

void Generator::dumpLL(const std::string& filename) {
    // mod->print(llvm::outs(), nullptr);
    std::error_code EC;
    llvm::raw_fd_ostream outFile(filename, EC);
    if (EC) {
        throw std::runtime_error("Could not open file " + filename + ": " + EC.message());
    }
    mod->print(outFile, nullptr);
}

