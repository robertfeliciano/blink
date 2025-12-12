#pragma once

#include <bridge/types.h>
#include <caml/mlvalues.h>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

using i128 = __int128;
using u128 = unsigned __int128;

struct Exp;

enum class UnOp { Neg, Not };

enum class BinOp { Add, Sub, Mul, Div, At, Mod, Pow, Shl, Lshr, Ashr, Eqeq, Neq, Lt, Lte, Gt, Gte, And, Or, Xor };

struct EBool {
    bool value;
    Ty   ty;
};
struct EInt {
    std::unique_ptr<IntTy> int_ty;
    union {
        i128 s;
        u128 u;
    };
    Ty ty;
};
struct EFloat {
    double  value;
    FloatTy float_ty;
    Ty      ty;
};
struct EStr {
    std::string value;
    Ty          ty;
};
struct EId {
    std::string id;
    Ty          ty;
};

struct ECall {
    std::unique_ptr<Exp>              callee;
    std::vector<std::unique_ptr<Exp>> args;
    Ty                                ty;
};

struct EBop {
    BinOp                op;
    std::unique_ptr<Exp> left;
    std::unique_ptr<Exp> right;
    Ty                   ty;
};

struct EUop {
    UnOp                 op;
    std::unique_ptr<Exp> arg;
    Ty                   ty;
};

struct EIndex {
    std::unique_ptr<Exp> collection;
    std::unique_ptr<Exp> index;
    Ty                   ty; // type of element, e.g. i32
};

struct EArray {
    std::vector<std::unique_ptr<Exp>> elements;
    Ty                                ty; // array type: [t x n], e.g. [i32 x 5]
};

struct ECast {
    std::unique_ptr<Exp> expr;
    Ty                   ty;
};

struct EProj {
    std::unique_ptr<Exp> obj;
    std::string          field;
    Ty                   ty;
};

struct EObjInit {
    std::string                                               id;
    std::vector<std::pair<std::string, std::unique_ptr<Exp>>> fields;
    Ty                                                        ty;
};

struct ENull {
    Ty ty;
};

using ExpVariant =
    std::variant<EBool, EInt, EFloat, EStr, EId, ECall, EBop, EUop, EIndex, EArray, ECast, EProj, EObjInit, ENull>;

struct Exp {
    ExpVariant val;
};

Exp convert_exp(value v);

template <typename... Alts, typename... Ts> constexpr bool holds_any_of(const std::variant<Ts...>& v) noexcept {
    return (std::holds_alternative<Alts>(v) || ...);
}