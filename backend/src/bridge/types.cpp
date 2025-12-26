#include <bridge/types.h>
#include <caml/mlvalues.h>
#include <iostream>
#include <stdexcept>
#include <util/constants.h>

Sint convert_sint(value v) {
    switch (Int_val(v)) {
        case Constants::SINT_Ti8:
            return Sint::Ti8;
        case Constants::SINT_Ti16:
            return Sint::Ti16;
        case Constants::SINT_Ti32:
            return Sint::Ti32;
        case Constants::SINT_Ti64:
            return Sint::Ti64;
        case Constants::SINT_Ti128:
            return Sint::Ti128;
        default:
            throw std::runtime_error("Unknown Signed Int variant");
    }
}

Uint convert_uint(value v) {
    switch (Int_val(v)) {
        case Constants::UINT_Tu8:
            return Uint::Tu8;
        case Constants::UINT_Tu16:
            return Uint::Tu16;
        case Constants::UINT_Tu32:
            return Uint::Tu32;
        case Constants::UINT_Tu64:
            return Uint::Tu64;
        case Constants::UINT_Tu128:
            return Uint::Tu128;
        default:
            throw std::runtime_error("Unknown Unsigned Int variant");
    }
}

FloatTy convert_float_ty(value v) {
    switch (Int_val(v)) {
        case Constants::FLOAT_F32:
            return FloatTy::Tf32;
        case Constants::FLOAT_F64:
            return FloatTy::Tf64;
        default:
            throw std::runtime_error("Unknown FloatTy variant");
    }
}

IntTy convert_int_ty(value v) {
    IntTy result;
    if (Tag_val(v) == Constants::INTTY_Signed) {
        result.tag  = IntTyTag::Signed;
        result.sint = convert_sint(Field(v, 0));
    } else {
        result.tag  = IntTyTag::Unsigned;
        result.uint = convert_uint(Field(v, 0));
    }
    return result;
}

RefTy convert_ref_ty(value v) {
    RefTy ref;
    if (Is_block(v)) {
        switch (Tag_val(v)) {
            case Constants::REFTY_Array: {
                ref.tag   = RefTyTag::RArray;
                ref.inner = std::make_unique<Ty>(convert_ty(Field(v, 0)));
                ref.size  = Int_val(Field(v, 1));
                break;
            }
            case Constants::REFTY_Class: {
                ref.tag   = RefTyTag::RClass;
                ref.cname = std::string(String_val(Field(v, 0)));
                break;
            }
            case Constants::REFTY_Fun: {
                ref.tag   = RefTyTag::RFun;
                value tys = Field(v, 0);
                while (tys != Val_emptylist) {
                    value head = Field(tys, 0);
                    ref.args.push_back(convert_ty(head));
                    tys = Field(tys, 1);
                }
                ref.ret = convert_ret_ty(Field(v, 1));
                break;
            }
            case Constants::REFTY_Ptr: {
                ref.tag = RefTyTag::RPtr;
                ref.pointedTy = std::make_unique<Ty>(convert_ty(Field(v, 0)));
                break;
            }
        }
    } else {
        ref.tag = RefTyTag::RString;
    }

    return ref;
}

Ty convert_ty(value v) {
    Ty ty;
    if (Is_block(v)) {
        switch (Tag_val(v)) {
            case Constants::TY_TInt:
                ty.tag    = TyTag::TInt;
                ty.int_ty = std::make_unique<IntTy>(convert_int_ty(Field(v, 0)));
                break;

            case Constants::TY_TFloat:
                ty.tag      = TyTag::TFloat;
                ty.float_ty = std::make_unique<FloatTy>(convert_float_ty(Field(v, 0)));
                break;

            case Constants::TY_TRef:
                ty.tag    = TyTag::TRef;
                ty.ref_ty = std::make_unique<RefTy>(convert_ref_ty(Field(v, 0)));
                break;
        }
    } else {
        ty.tag = TyTag::TBool;
    }
    return ty;
}

RetTy convert_ret_ty(value v) {
    RetTy ret;

    if (Is_block(v)) {
        ret.tag = RetTyTag::RetVal;
        ret.val = std::make_unique<Ty>(convert_ty(Field(v, 0)));
    } else {
        ret.tag = RetTyTag::RetVoid;
    }

    return ret;
}

bool is_obj_ty(const Ty& t) {
    return t.tag == TyTag::TRef && (t.ref_ty->tag == RefTyTag::RClass || t.ref_ty->tag == RefTyTag::RClass);
}