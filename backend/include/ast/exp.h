#pragma once

#include <vector>
#include <memory>
#include <string>
#include <variant>
#include <optional>

#include <caml/mlvalues.h>

#include <ast/node.h>

struct Stmt;

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

struct EInt  { int64_t value; };
struct EBool { bool value; };
struct EVar  { std::string id; };
struct EFloat { double value; };
struct EStr { std::string value; };
struct ECall { 
    std::unique_ptr<Node<Exp>> callee;
    std::vector<std::unique_ptr<Node<Exp>>> args;
};
struct EBop { 
    BinOp op;
    std::unique_ptr<Node<Exp>> left;
    std::unique_ptr<Node<Exp>> right;
};
struct EUop { 
    UnOp op;
    std::unique_ptr<Node<Exp>> arg;
};
struct EIndex {
    std::unique_ptr<Node<Exp>> collection;
    std::unique_ptr<Node<Exp>> index;
};
struct EArray {
    std::vector<std::unique_ptr<Node<Exp>>> elements;
};
struct ERange {
    std::unique_ptr<Node<Exp>> start;
    std::unique_ptr<Node<Exp>> end;
    bool inclusive;
};
struct ELambda { 
    std::vector<std::unique_ptr<Stmt>> body;
};

using ExpVariant = std::variant<
    EInt, 
    EBool, 
    EVar,
    EFloat,
    EStr,
    ECall,
    EBop,
    EUop,
    EIndex,
    EArray,
    ELambda,
    ERange>;

struct Exp {
    ExpVariant val;
};

Node<Exp> convert_exp_node(value v);

std::string toString(BinOp op);

std::string toString(UnOp op);

std::string expToString(const Exp& exp);