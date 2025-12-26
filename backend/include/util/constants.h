#pragma once

namespace Constants {
    enum SintTag { SINT_Ti8 = 0, SINT_Ti16 = 1, SINT_Ti32 = 2, SINT_Ti64 = 3, SINT_Ti128 = 4 };

    enum UintTag { UINT_Tu8 = 0, UINT_Tu16 = 1, UINT_Tu32 = 2, UINT_Tu64 = 3, UINT_Tu128 = 4 };

    enum FloatTyTag { FLOAT_F32 = 0, FLOAT_F64 = 1 };

    enum IntTyVariant { INTTY_Signed = 0, INTTY_Unsigned = 1 };

    enum RefTyVariant { REFTY_Array = 0, REFTY_Class = 1, REFTY_Fun = 2, REFTY_Ptr = 3 };

    enum TyVariant { TY_TInt = 0, TY_TFloat = 1, TY_TRef = 2 };

    enum ExpVariant {
        EXP_Bool    = 0,
        EXP_Int     = 1,
        EXP_Float   = 2,
        EXP_Str     = 3,
        EXP_Id      = 4,
        EXP_Call    = 5,
        EXP_Bop     = 6,
        EXP_Uop     = 7,
        EXP_Index   = 8,
        EXP_Array   = 9,
        EXP_Cast    = 10,
        EXP_Proj    = 11,
        EXP_ObjInit = 12,
        EXP_Lambda  = 13,
        EXP_Null    = 14
    };

    enum UnOpTag { UNOP_Neg = 0, UNOP_Not = 1, UNOP_BNeg = 2 };

    enum BinOpTag {
        BINOP_Add  = 0,
        BINOP_Sub  = 1,
        BINOP_Mul  = 2,
        BINOP_Div  = 3,
        BINOP_At   = 4,
        BINOP_Mod  = 5,
        BINOP_Pow  = 6,
        BINOP_Eqeq = 7,
        BINOP_Neq  = 8,
        BINOP_Lt   = 9,
        BINOP_Lte  = 10,
        BINOP_Gt   = 11,
        BINOP_Gte  = 12,
        BINOP_And  = 13,
        BINOP_Or   = 14,
        BINOP_Shl  = 15,
        BINOP_Lshr = 16,
        BINOP_Ashr = 17,
        BINOP_Xor  = 18,
        BINOP_BXor = 19,
        BINOP_BAnd = 20,
        BINOP_BOr  = 21
    };

    enum StmtVariant {
        STMT_Assn  = 0,
        STMT_Decl  = 1,
        STMT_Ret   = 2,
        STMT_SCall = 3,
        STMT_If    = 4,
        STMT_While = 5,
        STMT_Free  = 6
    };

    enum CtrlTag { CTRL_Break = 0, CTRL_Continue = 1 };
} // namespace Constants