#pragma once

enum class BinOp {
    ADD,    SUB,    MUL,    DIV,
    MOD,    POW,    AT,
    EQEQ,   NEQ,
    LT,     LTE,
    GT,     GTE,
    AND,    OR
};

enum class AOp {
    EQ,     PLUEQ,  MINEQ,  TIMEQ,
    DIVEQ,  ATEQ,   POWEQ,  MODEQ
};


enum class UnOp {
    NEG,    NOT
};