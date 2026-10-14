#pragma once

#include <bridge/decl.h>
#include <bridge/prog.h>
#include <unordered_map>
#include <unordered_set>

namespace llvm {
class Constant;
}

struct Generator;

class DeclToLLVisitor {
    Generator&                                  gen;
    std::unordered_map<std::string, const GDecl*> globalDeclarations;
    std::unordered_set<std::string>              initializingGlobals;

  public:
    explicit DeclToLLVisitor(Generator& g) : gen(g) {}

    void codegenCDecl(const CDecl& g);
    void codegenFDecl(const FDecl& d);
    void codegenGDecl(const GDecl& d);
    void codegenGlobals(const std::vector<GDecl>& globals);
    llvm::Constant* codegenGlobalInitializer(const std::string& id);

    void codegenFunctionPrototypes(const std::vector<Proto>& ps, const std::vector<FDecl>& fns);

  private:
    void codegenFunctionProto(const FDecl& fn);
    void codegenProto(const Proto& fn);
};
