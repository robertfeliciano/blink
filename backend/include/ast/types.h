#pragma once

#include <vector>
#include <memory>

#include <caml/mlvalues.h>

struct Ty;

enum class Sint {
    Ti8, Ti16, Ti32, Ti64, Ti128
};

enum class Uint {
    Tu8, Tu16, Tu32, Tu64, Tu128
};

enum class FloatTy {
    Tf32, Tf64
};

enum class IntTyTag { Signed, Unsigned };

struct IntTy {
    IntTyTag tag;
    union {
        Sint sint;
        Uint uint;
    };
};

enum class RetTyTag {
    RetVoid,
    RetVal
};

struct RetTy {
    RetTyTag tag;
    std::unique_ptr<Ty> val;
};


enum class RefTyTag { RString, RArray, RFun };

struct RefTy {
    RefTyTag tag;
    std::vector<Ty> args;         // for RFun
    RetTy ret;                    // for RFun
    std::unique_ptr<Ty> inner;    // for RArray
};

enum class TyTag {
    TBool,
    TInt,
    TFloat,
    TRef
};

struct Ty {
    TyTag tag;
    std::unique_ptr<IntTy> int_ty;
    std::unique_ptr<FloatTy> float_ty;
    std::unique_ptr<RefTy> ref_ty;
};

Ty convert_ty(value v);

RetTy convert_ret_ty(value v);

std::string tyToString(const Ty& ty);