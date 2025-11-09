#include <stdexcept>
#include <iostream>

#include <bridge/types.h>

Sint convert_sint(value v) {
    switch (Int_val(v)) {
        case 0: return Sint::Ti8;
        case 1: return Sint::Ti16;
        case 2: return Sint::Ti32;
        case 3: return Sint::Ti64;
        case 4: return Sint::Ti128;
        default: throw std::runtime_error("Unknown Signed Int variant");
    }
}

Uint convert_uint(value v) {
    switch (Int_val(v)) {
        case 0: return Uint::Tu8;
        case 1: return Uint::Tu16;
        case 2: return Uint::Tu32;
        case 3: return Uint::Tu64;
        case 4: return Uint::Tu128;
        default: throw std::runtime_error("Unknown Unsigned Int variant");
    }
}

FloatTy convert_float_ty(value v) {
    switch (Int_val(v)) {
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
    if (Is_block(v)) {
        switch (Tag_val(v)) {
            case 0: { 
                ref.tag = RefTyTag::RArray;
                ref.inner = std::make_unique<Ty>(convert_ty(Field(v, 0)));
                ref.size = Long_val(Field(v, 1));
                break;
            }
            case 1: { 
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
            case 2: {
                ref.tag = RefTyTag::RClass;
                ref.cname = std::string(String_val(Field(v, 0)));
                break;
            }
        }
    }
    else {
        ref.tag = RefTyTag::RString;
    }
    
    return ref;
}

Ty convert_ty(value v) {
    Ty ty;
    if (Is_block(v)) {
        switch (Tag_val(v)) {
            case 0: 
                ty.tag = TyTag::TInt;
                ty.int_ty = std::make_unique<IntTy>(convert_int_ty(Field(v, 0)));
                break;
    
            case 1: 
                ty.tag = TyTag::TFloat;
                ty.float_ty = std::make_unique<FloatTy>(convert_float_ty(Field(v, 0)));
                break;
    
            case 2: 
                ty.tag = TyTag::TRef;
                ty.ref_ty = std::make_unique<RefTy>(convert_ref_ty(Field(v, 0)));
                break;
        }
    } 
    else {
        ty.tag = TyTag::TBool;
    }
    return ty;
}

RetTy convert_ret_ty(value v) {
    RetTy ret;

    if (Is_block(v)) {
        ret.tag = RetTyTag::RetVal;
        ret.val = std::make_unique<Ty>(convert_ty(Field(v, 0)));
    } 
    else {
        ret.tag = RetTyTag::RetVoid;
    }
    
    return ret;
}

std::string tyToString(const Ty& ty) {
    switch (ty.tag) {
        case TyTag::TBool: return "bool";
        case TyTag::TInt: {
            switch (ty.int_ty->tag) {
                case IntTyTag::Signed: 
                    switch (ty.int_ty->sint) {
                        case Sint::Ti8: return "i8";
                        case Sint::Ti16: return "i16";
                        case Sint::Ti32: return "i32";
                        case Sint::Ti64: return "i64";
                        case Sint::Ti128: return "i128";
                    }
                case IntTyTag::Unsigned: 
                    switch (ty.int_ty->uint) {
                        case Uint::Tu8: return "u8";
                        case Uint::Tu16: return "u16";
                        case Uint::Tu32: return "u32";
                        case Uint::Tu64: return "u64";
                        case Uint::Tu128: return "u128";
                    }
            }
        }
        case TyTag::TFloat: {
            switch (*(ty.float_ty)) {
                case FloatTy::Tf32: return "f32";
                case FloatTy::Tf64: return "f64";
            }
        }
        case TyTag::TRef: {
            switch (ty.ref_ty->tag) {
                case RefTyTag::RString: return "string";
                case RefTyTag::RArray: 
                    return "array of " + tyToString(*(ty.ref_ty->inner));
                case RefTyTag::RFun: {
                    std::string s = "fn(";
                    for (size_t i = 0; i < ty.ref_ty->args.size(); ++i) {
                        if (i) s += ", ";
                        s += tyToString(ty.ref_ty->args[i]);
                    }
                    s += ") -> ";
                    switch (ty.ref_ty->ret.tag) {
                        case RetTyTag::RetVoid: s += "void"; break;
                        case RetTyTag::RetVal: s += tyToString(*(ty.ref_ty->ret.val)); break;
                    }
                    return s;
                }
                case RefTyTag::RClass: return "class(" + ty.ref_ty->cname + ")";
            }
        }
    }
    throw std::runtime_error("Unknown type");
}