#pragma once

#include <variant>
#include <string>

#include <ast/stmt.h>

struct GDecl {
    std::string name;
    Node<Exp> init;
};

struct FDecl {
    RetTy rtyp;
    std::string fname;
    std::vector<std::pair<Ty, std::string>> args;
    std::vector<Node<Stmt>> body;
};

using DeclVariant = std::variant<Node<GDecl>, Node<FDecl>>;

struct Decl {
    DeclVariant val;
};

Decl convert_decl(value v);