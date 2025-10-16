#pragma once

#include <vector>
#include <memory>
#include <string>
#include <variant>
#include <optional>

#include <caml/mlvalues.h>

#include <bridge/exp.h>
#include <bridge/types.h>

struct Stmt;

struct Assn {
    std::unique_ptr<Exp> lhs;
    std::unique_ptr<Exp> rhs;
    Ty ty;
};

struct VDecl {
    std::string id;
    Ty ty;
    std::unique_ptr<Exp> init;
    bool is_const;
};

struct Ret {
    std::optional<std::unique_ptr<Exp>> value;
};

struct SCall {
    std::unique_ptr<Exp> callee;
    std::vector<std::unique_ptr<Exp>> args;
};

struct If {
    std::unique_ptr<Exp> cond;
    std::vector<std::unique_ptr<Stmt>> then_branch;
    std::vector<std::unique_ptr<Stmt>> else_branch;
};

struct For {
    std::string id;
    std::unique_ptr<Exp> start;
    std::unique_ptr<Exp> end;
    std::vector<std::unique_ptr<Stmt>> body;
};

struct While {
    std::unique_ptr<Exp> cond;
    std::vector<std::unique_ptr<Stmt>> body;
};

struct Break {};
struct Continue {};

using StmtVariant = std::variant<
    Assn,
    VDecl,
    Ret,
    SCall,
    If,
    For,
    While,
    Break,
    Continue
>;

struct Stmt {
    StmtVariant val;
};

// Convert an OCaml value (typed AST stmt) into this bridge Stmt
Stmt convert_stmt(value v);

std::string stmtToString(const Stmt& stmt, int indentLevel = 0);
