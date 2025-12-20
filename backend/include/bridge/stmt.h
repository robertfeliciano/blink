#pragma once

#include <bridge/exp.h>
#include <bridge/types.h>
#include <caml/mlvalues.h>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

struct Stmt;

struct Assn {
    std::unique_ptr<Exp> lhs;
    std::unique_ptr<Exp> rhs;
    Ty                   ty;
};

struct LambdaDecl {
    std::string          id;
    RefTy                refTy;
    std::unique_ptr<Exp> lambda;
};

struct VDecl {
    std::string          id;
    Ty                   ty;
    std::unique_ptr<Exp> init;
    bool                 is_const;
};

struct Ret {
    std::optional<std::shared_ptr<Exp>> value;
};

struct SCall {
    std::unique_ptr<Exp>              callee;
    std::vector<std::unique_ptr<Exp>> args;
};

struct If {
    std::unique_ptr<Exp>               cond;
    std::vector<std::unique_ptr<Stmt>> then_branch;
    std::vector<std::unique_ptr<Stmt>> else_branch;
};

struct While {
    std::unique_ptr<Exp>               cond;
    std::vector<std::unique_ptr<Stmt>> body;
};

struct Free {
    std::vector<std::unique_ptr<Exp>>  exps;
};

struct Break {};
struct Continue {};

// TODO do lambdadecl

using StmtVariant = std::variant<Assn, VDecl, Ret, SCall, If, While, Free, Break, Continue>;

struct Stmt {
    StmtVariant val;
};

Stmt convert_stmt(value v);
