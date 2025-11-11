#include <stdexcept>
#include <iostream>

#include <caml/mlvalues.h>
#include <caml/custom.h> 
#include <gmp.h>
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

EInt convert_zarith_int(value z_val, value int_ty_val) {
    if (!z_val)
        throw std::runtime_error("Null Z.t value in convert_zarith_int");

    // Convert OCaml int_ty first
    auto int_ty = std::make_unique<IntTy>(convert_int_ty(int_ty_val));

    // Extract mpz_t pointer from Zarith custom block
    mpz_t* z_ptr = reinterpret_cast<mpz_t*>(Data_custom_val(z_val));
    if (!z_ptr)
        throw std::runtime_error("Invalid Zarith integer pointer");

    // Determine signedness from IntTy (customize as needed)
    bool is_signed = true;
    if (int_ty && int_ty->tag == IntTyTag::Unsigned) {
        is_signed = false;
    }

    // Check bit length to avoid overflow
    size_t bitlen = mpz_sizeinbase(*z_ptr, 2);
    if (bitlen > 128) {
        throw std::overflow_error("Zarith integer too large for 128-bit target");
    }

    // Export raw bytes (big-endian)
    unsigned char buf[16] = {0};
    size_t written = 0;
    mpz_export(buf, &written, 1 /* most significant first */, 1, 1, 0, *z_ptr);

    // Combine bytes into unsigned __int128
    unsigned __int128 mag = 0;
    for (size_t i = 0; i < written; ++i)
        mag = (mag << 8) | buf[i];

    // Build EInt
    EInt ei;
    ei.int_ty = std::move(int_ty);

    if (is_signed) {
        if (mpz_sgn(*z_ptr) < 0) {
            ei.s = -static_cast<__int128>(mag);
        } else {
            ei.s = static_cast<__int128>(mag);
        }
    } else {
        if (mpz_sgn(*z_ptr) < 0)
            throw std::overflow_error("Negative value cannot be stored in unsigned int");
        ei.u = mag;
    }

    return ei;
}


Exp convert_exp(value v) {
    Exp result;

    if (!Is_block(v)) {
        throw std::runtime_error("Expected block for exp variant");
    }

    switch (Tag_val(v)) {
        case 0: { // Bool of bool
            bool b = Bool_val(Field(v, 0));
            result.val = EBool{b};
            break;
        }
        case 1: { // Int of Z.t * int_ty
            value z_val = Field(v, 0);
            value int_ty_val = Field(v, 1);

            EInt ei = convert_zarith_int(z_val, int_ty_val);
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

std::string toString(BinOp op) {
    switch (op) {
        case BinOp::Add: return "+";
        case BinOp::Sub: return "-";
        case BinOp::Mul: return "*";
        case BinOp::Div: return "/";
        case BinOp::At: return "@";
        case BinOp::Mod: return "%";
        case BinOp::Pow: return "**";
        case BinOp::Eqeq: return "==";
        case BinOp::Neq: return "!=";
        case BinOp::Lt: return "<";
        case BinOp::Lte: return "<=";
        case BinOp::Gt: return ">";
        case BinOp::Gte: return ">=";
        case BinOp::And: return "&&";
        case BinOp::Or: return "||";
    }
    throw std::runtime_error("Unknown BinOp");
}

std::string toString(UnOp op) {
    switch (op) {
        case UnOp::Neg: return "-";
        case UnOp::Not: return "!";
    }
    throw std::runtime_error("Unknown UnOp");
}


// Helper visitor for expToString
struct ExpToStringVisitor {
    std::string operator()(const EBool& e) const { return e.value ? "true" : "false"; }
    std::string operator()(const EInt& e) const {
        // We don't have the bigint value here; print type and TODO marker
        return std::string("<int TODO>");
    }
    std::string operator()(const EFloat& e) const { return std::to_string(e.value); }
    std::string operator()(const EStr& e) const { return "\"" + e.value + "\""; }
    std::string operator()(const EId& e) const { return e.id; }
    std::string operator()(const ECall& e) const {
        std::string res = expToString(*e.callee) + "(";
        for (size_t i = 0; i < e.args.size(); ++i) {
            res += expToString(*e.args[i]);
            if (i + 1 < e.args.size()) res += ", ";
        }
        res += ")";
        return res;
    }
    std::string operator()(const EBop& e) const {
        return "(" + expToString(*e.left) + " " + toString(e.op) + " " + expToString(*e.right) + ")";
    }
    std::string operator()(const EUop& e) const {
        return "(" + toString(e.op) + " " + expToString(*e.arg) + ")";
    }
    std::string operator()(const EIndex& e) const {
        return expToString(*e.collection) + "[" + expToString(*e.index) + "]";
    }
    std::string operator()(const EArray& e) const {
        std::string res = "[";
        for (size_t i = 0; i < e.elements.size(); ++i) {
            res += expToString(*e.elements[i]);
            if (i + 1 < e.elements.size()) res += ", ";
        }
        res += "]";
        return res;
    }
    std::string operator()(const ECast& e) const {
        return "(" + expToString(*e.expr) + " as " + tyToString(e.ty) + ")";
    }
    std::string operator()(const EProj& e) const {
        return expToString(*e.obj) + "." + e.field;
    }
    std::string operator()(const EObjInit& e) const {
        std::string res = e.id + "{";
        for (size_t i = 0; i < e.fields.size(); ++i) {
            res += e.fields[i].first + ": " + expToString(*e.fields[i].second);
            if (i + 1 < e.fields.size()) res += ", ";
        }
        res += "}";
        return res;
    }
};

std::string expToString(const Exp& exp) {
    return std::visit(ExpToStringVisitor{}, exp.val);
}