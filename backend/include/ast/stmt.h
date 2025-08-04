#pragma once

#include <vector>
#include <memory>
#include <string>
#include <variant>
#include <optional>

#include <caml/mlvalues.h>

#include <ast/exp.h>
#include <ast/types.h>

struct Stmt;

struct Assn {
    std::unique_ptr<Node<Exp>> lhs;
    std::unique_ptr<Node<Exp>> rhs;
};

struct VDecl {
    std::string id;
    Ty ty;
    std::unique_ptr<Node<Exp>> init;
    bool is_const;
};

struct Ret {
    std::optional<std::shared_ptr<Node<Exp>>> value;
};

struct SCall {
    std::unique_ptr<Node<Exp>> callee;
    std::vector<std::unique_ptr<Node<Exp>>> args;
};

struct If {
    std::unique_ptr<Node<Exp>> cond;
    std::vector<std::unique_ptr<Node<Stmt>>> then_branch;
    std::vector<std::unique_ptr<Node<Stmt>>> else_branch;
};

// do not need a For struct since desugaring will make For into While

struct While {
    std::unique_ptr<Node<Exp>> cond;
    std::vector<std::unique_ptr<Node<Stmt>>> body;
};

struct Break {};

struct Continue {};

using StmtVariant = std::variant<
    Assn,
    VDecl,
    Ret,
    SCall,
    If,
    While,
    Break,
    Continue>;

struct Stmt {
    StmtVariant val;
};

Node<Stmt> convert_stmt_node(value v);

std::string stmtToString(const Stmt& stmt, int indentLevel);