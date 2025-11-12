#pragma once

#include <vector>
#include <memory>
#include <string>
#include <variant>
#include <optional>

#include <caml/mlvalues.h>

#include <bridge/types.h>

using i128 = __int128;
using u128 = unsigned __int128;

struct Exp;

enum class UnOp {
    Neg,
    Not
};

enum class BinOp {
    Add,
    Sub,
    Mul,
    Div,
    At,
    Mod,
    Pow,
    Eqeq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or
};

struct EBool { bool value; };
struct EInt  { 
    std::unique_ptr<IntTy> int_ty;
    union {
        i128 s;
        u128 u;
    };
};
struct EFloat { double value; FloatTy float_ty; };
struct EStr  { std::string value; };
struct EId   { std::string id; };

struct ECall {
    std::unique_ptr<Exp> callee;
    std::vector<std::unique_ptr<Exp>> args;
    Ty ty;
};

struct EBop {
    BinOp op;
    std::unique_ptr<Exp> left;
    std::unique_ptr<Exp> right;
    Ty ty;
};

struct EUop {
    UnOp op;
    std::unique_ptr<Exp> arg;
    Ty ty;
};

struct EIndex {
    std::unique_ptr<Exp> collection;
    std::unique_ptr<Exp> index;
    Ty ty;
};

struct EArray {
    std::vector<std::unique_ptr<Exp>> elements;
    Ty ty;
    uint64_t size = 0;
};

struct ECast {
    std::unique_ptr<Exp> expr;
    Ty ty;
};

struct EProj {
    std::unique_ptr<Exp> obj;
    std::string field;
};

struct EObjInit {
    std::string id;
    std::vector<std::pair<std::string, std::unique_ptr<Exp>>> fields;
};

using ExpVariant = std::variant<
    EBool,
    EInt,
    EFloat,
    EStr,
    EId,
    ECall,
    EBop,
    EUop,
    EIndex,
    EArray,
    ECast,
    EProj,
    EObjInit
>;

struct Exp {
    ExpVariant val;
};

Exp convert_exp(value v);
