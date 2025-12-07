#pragma once

#include <caml/mlvalues.h>
#include <memory>
#include <string>
#include <vector>

struct Ty;

enum class Sint { Ti8, Ti16, Ti32, Ti64, Ti128 };

enum class Uint { Tu8, Tu16, Tu32, Tu64, Tu128 };

enum class FloatTy { Tf32, Tf64 };

enum class IntTyTag { Signed, Unsigned };

struct IntTy {
    IntTyTag tag;
    union {
        Sint sint;
        Uint uint;
    };
};

enum class RetTyTag { RetVoid, RetVal };

struct RetTy {
    RetTyTag            tag;
    std::unique_ptr<Ty> val; // for RetVal
};

enum class RefTyTag { RString, RArray, RClass, RFun };

struct RefTy {
    RefTyTag tag;

    // for RFun
    std::vector<Ty> args;
    RetTy           ret;

    // for RArray
    std::unique_ptr<Ty> inner;
    int                 size = 0;

    // for RClass
    std::string cname;
};

enum class TyTag { TBool, TInt, TFloat, TRef };

struct Ty {
    TyTag                    tag;
    std::unique_ptr<IntTy>   int_ty;
    std::unique_ptr<FloatTy> float_ty;
    std::unique_ptr<RefTy>   ref_ty;
};

IntTy convert_int_ty(value v);

FloatTy convert_float_ty(value v);

Ty convert_ty(value v);

RetTy convert_ret_ty(value v);

bool is_obj_ty(const Ty& t);