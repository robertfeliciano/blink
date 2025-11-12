#include <stdexcept>
#include <iostream>

#include <caml/mlvalues.h>
#include <caml/custom.h> 
#include <cstdlib>
#include <vector>
#include <cstring>
#include <limits>

#include <bridge/stmt.h>
#include <bridge/exp.h>
#include <bridge/types.h>

UnOp convert_unop(value v) {
    switch (Int_val(v)) {
        case 0: return UnOp::Neg;
        case 1: return UnOp::Not;
        default: throw std::runtime_error("Unknown UnOp");
    }
}

BinOp convert_binop(value v) {
    switch (Int_val(v)) {
        case 0: return BinOp::Add;
        case 1: return BinOp::Sub;
        case 2: return BinOp::Mul;
        case 3: return BinOp::Div;
        case 4: return BinOp::At;
        case 5: return BinOp::Mod;
        case 6: return BinOp::Pow;
        case 7: return BinOp::Eqeq;
        case 8: return BinOp::Neq;
        case 9: return BinOp::Lt;
        case 10: return BinOp::Lte;
        case 11: return BinOp::Gt;
        case 12: return BinOp::Gte;
        case 13: return BinOp::And;
        case 14: return BinOp::Or;
        default: throw std::runtime_error("Unknown BinOp");
    }
}

i128 string_to_i128(const std::string& str) {
    size_t i = 0;

    bool negative = false;
    if (str[i] == '-') { 
        negative = true; i++; 
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
        if (!isdigit(str[i])) throw std::invalid_argument("Invalid character in input");
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
        case 0: { // Bool of bool
            bool b = Bool_val(Field(v, 0));
            result.val = EBool{b};
            break;
        }
        case 1: { // Int of string * int_ty
            std::string maybe_z = String_val(Field(v, 0));
            value maybe_int_ty = Field(v, 1);

            auto int_ty = convert_int_ty(maybe_int_ty);

            EInt ei;
            ei.int_ty = std::make_unique<IntTy>(int_ty);

            if (int_ty.tag == IntTyTag::Signed) {
                ei.s = string_to_i128(maybe_z);
            } else {
                ei.u = string_to_u128(maybe_z);
            }
        
            result.val = std::move(ei);
            break;
        }
        
        case 2: { // Float of float * float_ty
            double d = Double_val(Field(v, 0));
            FloatTy fty = convert_float_ty(Field(v, 1));
            result.val = EFloat{d, fty};
            break;
        }
        case 3: { // Str of string
            std::string s = String_val(Field(v, 0));
            result.val = EStr{s};
            break;
        }
        case 4: { // Id of id
            std::string id = String_val(Field(v, 0));
            result.val = EId{id};
            break;
        }
        case 5: { // Call of exp * exp list * ty
            value callee_v = Field(v, 0);
            value args_v = Field(v, 1);
            value ty_v = Field(v, 2);

            auto callee = std::make_unique<Exp>(convert_exp(callee_v));
            std::vector<std::unique_ptr<Exp>> args;
            while (args_v != Val_emptylist) {
                value head = Field(args_v, 0);
                args.push_back(std::make_unique<Exp>(convert_exp(head)));
                args_v = Field(args_v, 1);
            }

            Ty ty = convert_ty(ty_v);
            result.val = ECall{ std::move(callee), std::move(args), std::move(ty) };
            break;
        }
        case 6: { // Bop of binop * exp * exp * ty
            BinOp bop = convert_binop(Field(v, 0));
            auto left = std::make_unique<Exp>(convert_exp(Field(v, 1)));
            auto right = std::make_unique<Exp>(convert_exp(Field(v, 2)));
            Ty ty = convert_ty(Field(v, 3));

            result.val = EBop{ bop, std::move(left), std::move(right), std::move(ty) };
            break;
        }
        case 7: { // Uop of unop * exp * ty
            UnOp uop = convert_unop(Field(v, 0));
            auto arg = std::make_unique<Exp>(convert_exp(Field(v, 1)));
            Ty ty = convert_ty(Field(v, 2));

            result.val = EUop{ uop, std::move(arg), std::move(ty) };
            break;
        }
        case 8: { // Index of exp * exp * ty
            auto collection = std::make_unique<Exp>(convert_exp(Field(v, 0)));
            auto index = std::make_unique<Exp>(convert_exp(Field(v, 1)));
            Ty ty = convert_ty(Field(v, 2));

            result.val = EIndex{ std::move(collection), std::move(index), std::move(ty) };
            break;
        }
        case 9: { // Array of exp list * ty * int64
            value elems_v = Field(v, 0);
            std::vector<std::unique_ptr<Exp>> elements;
            while (elems_v != Val_emptylist) {
                value head = Field(elems_v, 0);
                elements.push_back(std::make_unique<Exp>(convert_exp(head)));
                elems_v = Field(elems_v, 1);
            }
            Ty ty = convert_ty(Field(v, 1));
            uint64_t size = static_cast<uint64_t>(Long_val(Field(v, 2)));

            EArray ea;
            ea.elements = std::move(elements);
            ea.ty = std::move(ty);
            ea.size = size;
            result.val = std::move(ea);
            break;
        }
        case 10: { // Cast of exp * ty
            auto expr = std::make_unique<Exp>(convert_exp(Field(v, 0)));
            Ty ty = convert_ty(Field(v, 1));
            result.val = ECast{ std::move(expr), std::move(ty) };
            break;
        }
        case 11: { // Proj of exp * id
            auto obj = std::make_unique<Exp>(convert_exp(Field(v, 0)));
            std::string field = String_val(Field(v, 1));
            result.val = EProj{ std::move(obj), field };
            break;
        }
        case 12: { // ObjInit of id * (id * exp) list
            std::string id = String_val(Field(v, 0));
            value fields_v = Field(v, 1);
            std::vector<std::pair<std::string, std::unique_ptr<Exp>>> fields;
            while (fields_v != Val_emptylist) {
                value pair = Field(fields_v, 0);
                std::string fname = String_val(Field(pair, 0));
                value fexp = Field(pair, 1);
                fields.emplace_back(fname, std::make_unique<Exp>(convert_exp(fexp)));
                fields_v = Field(v, 1);
            }
            result.val = EObjInit{ id, std::move(fields) };
            break;
        }
        case 13: { // Lambda of (id * ty) list * ret_ty * block
            throw std::runtime_error("Lambdas not supported yet!");
        }
        default: {
            throw std::runtime_error("Unsupported exp variant in bridge conversion");
        }
    }

    return result;
}