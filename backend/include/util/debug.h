#include "llvm/Support/raw_ostream.h"

#include <cstdio>

#define PRINT_VAR_TYPE(var)                                                                                            \
    do {                                                                                                               \
        llvm::outs() << "type of " << #var << ": ";                                                                    \
        (var)->print(llvm::outs());                                                                                    \
        llvm::outs() << "\n";                                                                                          \
        llvm::outs().flush(); /* write immediately */ \
    } while (0)