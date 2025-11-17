#pragma once

#include "llvm/IR/Value.h"

#include <bridge/exp.h>

using llvm::Value;

struct Generator;

class ExpToLLVisitor {
    Generator& gen;

public:
    explicit ExpToLLVisitor(Generator& g) : gen(g) {}

    Value* operator()(const EInt& e);
    Value* operator()(const EBool& e);
    Value* operator()(const EId& e);
    Value* operator()(const EFloat& e);
    Value* operator()(const EStr& e);
    Value* operator()(const ECall& e);
    Value* operator()(const EBop& e);
    Value* operator()(const EUop& e);
    Value* operator()(const EIndex& e);
    Value* operator()(const EArray& e);

private:
    const Ty& getExpTy(const Exp& exp) {
        return std::visit([](const auto& node) -> const Ty& {
            return node.ty;
        }, exp.val);
    };
};