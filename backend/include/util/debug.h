#include "llvm/Support/raw_ostream.h"

#include <cstdio>

#define PRINT_VAR_TYPE(var)                                                                                            \
    do {                                                                                                               \
        printf("type of %s: ", #var);                                                                                  \
        (var)->print(llvm::outs());                                                                                    \
        puts("\n");                                                                                                    \
    } while (0)
