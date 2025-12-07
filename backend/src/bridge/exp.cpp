#include <bridge/exp.h>
#include <bridge/stmt.h>
#include <bridge/types.h>
#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <util/constants.h>
#include <vector>

UnOp convert_unop(value v) {
    switch (Int_val(v)) {
        case Constants::UNOP_Neg:
            return UnOp::Neg;
        case Constants::UNOP_Not:
            return UnOp::Not;
        default:
            throw std::runtime_error("Unknown UnOp");
    }
}

BinOp convert_binop(value v) {
    switch (Int_val(v)) {
        case Constants::BINOP_Add:
            return BinOp::Add;
        case Constants::BINOP_Sub:
            return BinOp::Sub;
        case Constants::BINOP_Mul:
            return BinOp::Mul;
        case Constants::BINOP_Div:
            return BinOp::Div;
        case Constants::BINOP_At:
            return BinOp::At;
        case Constants::BINOP_Mod:
            return BinOp::Mod;
        case Constants::BINOP_Pow:
            return BinOp::Pow;
        case Constants::BINOP_Eqeq:
            return BinOp::Eqeq;
        case Constants::BINOP_Neq:
            return BinOp::Neq;
        case Constants::BINOP_Lt:
            return BinOp::Lt;
        case Constants::BINOP_Lte:
            return BinOp::Lte;
        case Constants::BINOP_Gt:
            return BinOp::Gt;
        case Constants::BINOP_Gte:
            return BinOp::Gte;
        case Constants::BINOP_And:
            return BinOp::And;
        case Constants::BINOP_Or:
            return BinOp::Or;
        default:
            throw std::runtime_error("Unknown BinOp");
    }
}

i128 string_to_i128(const std::string& str) {
    size_t i = 0;

    bool negative = false;
    if (str[i] == '-') {
        negative = true;
        i++;
    }

    i128 result = 0;
    for (; i < str.size(); ++i) {
        if (!isdigit(str[i]))
            throw std::invalid_argument("Invalid character in input");
        result = result * 10 + (str[i] - '0');
    }

    return negative ? -result : result;
}

u128 string_to_u128(const std::string& str) {
    size_t i = 0;
    if (str[i] == '-')
        throw std::invalid_argument("Unsigned integer cannot have sign");

    u128 result = 0;
    for (; i < str.size(); ++i) {
        if (!isdigit(str[i]))
            throw std::invalid_argument("Invalid character in input");
        result = result * 10 + (str[i] - '0');
    }

    return result;
}

Exp convert_exp(value v) {
    Exp result;

    if (!Is_block(v)) {
        throw std::runtime_error("Expected caml block for exp variant");
    }

    switch (Tag_val(v)) {
        case Constants::EXP_Bool: { // Bool of bool
            bool b = Bool_val(Field(v, 0));
            Ty   ty;
            ty.tag     = TyTag::TBool;
            result.val = EBool{b, std::move(ty)};
            break;
        }
        case Constants::EXP_Int: { // Int of string * int_ty
            std::string maybe_z      = String_val(Field(v, 0));
            value       maybe_int_ty = Field(v, 1);

            auto int_ty = convert_int_ty(maybe_int_ty);

            Ty ty;
            ty.tag    = TyTag::TInt;
            ty.int_ty = std::make_unique<IntTy>(int_ty);

            EInt ei;
            ei.int_ty = std::make_unique<IntTy>(int_ty);
            ei.ty     = std::move(ty);

            if (int_ty.tag == IntTyTag::Signed)
                ei.s = string_to_i128(maybe_z);
            else
                ei.u = string_to_u128(maybe_z);

            result.val = std::move(ei);
            break;
        }

        case Constants::EXP_Float: { // Float of float * float_ty
            double  d   = Double_val(Field(v, 0));
            FloatTy fty = convert_float_ty(Field(v, 1));

            Ty ty;
            ty.tag      = TyTag::TFloat;
            ty.float_ty = std::make_unique<FloatTy>(fty);

            result.val = EFloat{d, fty, std::move(ty)};
            break;
        }
        case Constants::EXP_Str: { // Str of string
            std::string s = String_val(Field(v, 0));

            Ty ty;
            ty.tag         = TyTag::TRef;
            ty.ref_ty      = std::make_unique<RefTy>();
            ty.ref_ty->tag = RefTyTag::RString;

            result.val = EStr{s, std::move(ty)};
            break;
        }
        case Constants::EXP_Id: { // Id of id
            std::string id = String_val(Field(v, 0));
            Ty          ty = convert_ty(Field(v, 1));
            result.val     = EId{id, std::move(ty)};
            break;
        }
        case Constants::EXP_Call: { // Call of exp * exp list * ty
            value callee_v = Field(v, 0);
            value args_v   = Field(v, 1);
            value ty_v     = Field(v, 2);

            auto                              callee = std::make_unique<Exp>(convert_exp(callee_v));
            std::vector<std::unique_ptr<Exp>> args;
            while (args_v != Val_emptylist) {
                value head = Field(args_v, 0);
                args.push_back(std::make_unique<Exp>(convert_exp(head)));
                args_v = Field(args_v, 1);
            }

            Ty ty      = convert_ty(ty_v);
            result.val = ECall{std::move(callee), std::move(args), std::move(ty)};
            break;
        }
        case Constants::EXP_Bop: { // Bop of binop * exp * exp * ty
            BinOp bop   = convert_binop(Field(v, 0));
            auto  left  = std::make_unique<Exp>(convert_exp(Field(v, 1)));
            auto  right = std::make_unique<Exp>(convert_exp(Field(v, 2)));
            Ty    ty    = convert_ty(Field(v, 3));

            result.val = EBop{bop, std::move(left), std::move(right), std::move(ty)};
            break;
        }
        case Constants::EXP_Uop: { // Uop of unop * exp * ty
            UnOp uop = convert_unop(Field(v, 0));
            auto arg = std::make_unique<Exp>(convert_exp(Field(v, 1)));
            Ty   ty  = convert_ty(Field(v, 2));

            result.val = EUop{uop, std::move(arg), std::move(ty)};
            break;
        }
        case Constants::EXP_Index: { // Index of exp * exp * ty
            auto collection = std::make_unique<Exp>(convert_exp(Field(v, 0)));
            auto index      = std::make_unique<Exp>(convert_exp(Field(v, 1)));
            Ty   ty         = convert_ty(Field(v, 2));

            result.val = EIndex{std::move(collection), std::move(index), std::move(ty)};
            break;
        }
        case Constants::EXP_Array: { // Array of exp list * ty
            value                             elems_v = Field(v, 0);
            std::vector<std::unique_ptr<Exp>> elements;
            while (elems_v != Val_emptylist) {
                value head = Field(elems_v, 0);
                elements.push_back(std::make_unique<Exp>(convert_exp(head)));
                elems_v = Field(elems_v, 1);
            }
            Ty ty = convert_ty(Field(v, 1));

            EArray ea;
            ea.elements = std::move(elements);
            ea.ty       = std::move(ty);
            result.val  = std::move(ea);
            break;
        }
        case Constants::EXP_Cast: { // Cast of exp * ty
            auto expr  = std::make_unique<Exp>(convert_exp(Field(v, 0)));
            Ty   ty    = convert_ty(Field(v, 1));
            result.val = ECast{std::move(expr), std::move(ty)};
            break;
        }
        case Constants::EXP_Proj: { // Proj of exp * id
            auto        obj   = std::make_unique<Exp>(convert_exp(Field(v, 0)));
            std::string field = String_val(Field(v, 1));
            Ty          ty    = convert_ty(Field(v, 2));
            result.val        = EProj{std::move(obj), field, std::move(ty)};
            break;
        }
        case Constants::EXP_ObjInit: { // ObjInit of id * (id * exp) list
            std::string id       = String_val(Field(v, 0));
            value       fields_v = Field(v, 1);

            std::vector<std::pair<std::string, std::unique_ptr<Exp>>> fields;
            while (fields_v != Val_emptylist) {
                value       pair  = Field(fields_v, 0);
                std::string fname = String_val(Field(pair, 0));
                value       fexp  = Field(pair, 1);
                fields.emplace_back(fname, std::make_unique<Exp>(convert_exp(fexp)));
                fields_v = Field(fields_v, 1);
            }

            Ty ty;
            ty.tag           = TyTag::TRef;
            ty.ref_ty        = std::make_unique<RefTy>();
            ty.ref_ty->tag   = RefTyTag::RClass;
            ty.ref_ty->cname = id;

            result.val = EObjInit{id, std::move(fields), std::move(ty)};
            break;
        }
        case Constants::EXP_Lambda: { // Lambda of (id * ty) list * ret_ty * block
            throw std::runtime_error("Lambdas not supported yet!");
        }
        case Constants::EXP_Null: { // Null of ref_ty
            Ty null_ty = convert_ty(Field(v, 0));
            result.val = ENull{.ty = std::move(null_ty)};
            break;
        }
        default: {
            throw std::runtime_error("Unsupported exp variant in bridge conversion");
        }
    }

    return result;
}