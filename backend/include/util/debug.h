#include <cstdio>
#include "llvm/Support/raw_ostream.h"

#define PRINT_VAR_TYPE(var)                          \
    do {                                             \
        printf("type of %s: ", #var);                \
        (var)->print(llvm::outs());                  \
        puts("\n");                                  \
    } while (0)
