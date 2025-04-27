#include <stdexcept>

#include <caml/mlvalues.h>

#include <ast/types.h>

Sint convert_sint(value v) {
    switch (Tag_val(v)) {
        case 0: return Sint::Ti8;
        case 1: return Sint::Ti16;
        case 2: return Sint::Ti32;
        case 3: return Sint::Ti64;
        case 4: return Sint::Ti128;
        default: throw std::runtime_error("Unknown Signed Int variant");
    }
}

Uint convert_uint(value v) {
    switch (Tag_val(v)) {
        case 0: return Uint::Tu8;
        case 1: return Uint::Tu16;
        case 2: return Uint::Tu32;
        case 3: return Uint::Tu64;
        case 4: return Uint::Tu128;
        default: throw std::runtime_error("Unknown Unsigned Int variant");
    }
}

FloatTy convert_float_ty(value v) {
    switch (Tag_val(v)) {
        case 0: return FloatTy::Tf32;
        case 1: return FloatTy::Tf64;
        default: throw std::runtime_error("Unknown FloatTy variant");
    }
}

IntTy convert_int_ty(value v) {
    IntTy result;
    if (Tag_val(v) == 0) {
        result.tag = IntTyTag::Signed;
        result.sint = convert_sint(Field(v, 0));
    } else {
        result.tag = IntTyTag::Unsigned;
        result.uint = convert_uint(Field(v, 0));
    }
    return result;
}

RefTy convert_ref_ty(value v) {
    RefTy ref;

    switch (Tag_val(v)) {
        case 0: { // RString
            ref.tag = RefTyTag::RString;
            break;
        }
        case 1: { // RArray of ty
            ref.tag = RefTyTag::RArray;
            ref.inner = std::make_unique<Ty>(convert_ty(Field(v, 0)));
            break;
        }
        case 2: { // RFun of ty list * ret_ty
            ref.tag = RefTyTag::RFun;
            value tys = Field(v, 0);
            while (tys != Val_emptylist) {
                value head = Field(tys, 0);
                ref.args.push_back(convert_ty(head));
                tys = Field(tys, 1);
            }
            ref.ret = convert_ret_ty(Field(v, 1));
            break;
        }
    }

    return ref;
}

Ty convert_ty(value v) {
    Ty ty;

    switch (Tag_val(v)) {
        case 0: // TBool
            ty.tag = TyTag::TBool;
            break;

        case 1: // TInt of int_ty
            ty.tag = TyTag::TInt;
            ty.int_ty = std::make_unique<IntTy>(convert_int_ty(Field(v, 0)));
            break;

        case 2: // TFloat of float_ty
            ty.tag = TyTag::TFloat;
            ty.float_ty = std::make_unique<FloatTy>(convert_float_ty(Field(v, 0)));
            break;

        case 3: // TRef of ref_ty
            ty.tag = TyTag::TRef;
            ty.ref_ty = std::make_unique<RefTy>(convert_ref_ty(Field(v, 0)));
            break;
    }

    return ty;
}

RetTy convert_ret_ty(value v) {
    RetTy ret;

    switch (Tag_val(v)) {
        case 0: // RetVoid
            ret.tag = RetTyTag::RetVoid;
            break;

        case 1: // RetVal of ty
            ret.tag = RetTyTag::RetVal;
            ret.val = std::make_unique<Ty>(convert_ty(Field(v, 0)));
            break;
    }

    return ret;
}