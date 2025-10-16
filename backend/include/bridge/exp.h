#pragma once

#include <vector>
#include <memory>
#include <string>
#include <variant>
#include <optional>

#include <caml/mlvalues.h>

#include <bridge/types.h>

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

// Basic literal/leaf variants
struct EBool { bool value; };
struct EInt  { std::string value; std::unique_ptr<IntTy> int_ty; };
struct EFloat { double value; FloatTy float_ty; };
struct EStr  { std::string value; };
struct EId   { std::string id; };

// Complex expressions
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

struct ERange {
    std::unique_ptr<Exp> start;
    std::unique_ptr<Exp> end;
    bool inclusive;
};

struct EProj {
    std::unique_ptr<Exp> obj;
    std::string field;
};

struct EObjCons {
    std::string id;
    std::vector<std::unique_ptr<Exp>> args;
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
    ERange,
    EProj,
    EObjCons,
    EObjInit
>;

struct Exp {
    ExpVariant val;
};

// Convert an OCaml value (typed AST exp) into this bridge Exp
Exp convert_exp(value v);

std::string toString(BinOp op);

std::string toString(UnOp op);

std::string expToString(const Exp& exp);
