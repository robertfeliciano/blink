#pragma once

#include "llvm/IR/Value.h"

#include <bridge/exp.h>

using llvm::Value;

struct Generator;

class LValueCreator {
    Generator& gen;

public:
    explicit LValueCreator(Generator& g) : gen(g) {}

    Value* codegenLValue(const Exp& e);
    Value* getStructFieldPtr(const EProj& e);
    Value* getArrayElemPtr(const EIndex& e);
};