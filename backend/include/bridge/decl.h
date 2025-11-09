#pragma once

#include <vector>
#include <string>
#include <memory>

#include <caml/mlvalues.h>

#include <bridge/stmt.h>
#include <bridge/exp.h>
#include <bridge/types.h>

struct FDecl {
    RetTy frtyp;
    std::string fname;
    std::vector<std::pair<Ty, std::string>> args;
    std::vector<std::unique_ptr<Stmt>> body;
};

struct Field {
    std::vector<std::unique_ptr<Stmt>> prelude;
    std::string fieldName;
    Ty ftyp;
    std::unique_ptr<Exp> init;
};

struct CDecl {
    std::string cname;
    std::vector<std::string> impls;
    std::vector<Field> fields;
    std::vector<FDecl> methods;
};

// Converters from OCaml values into bridge representations
FDecl convert_fdecl(value v);
Field convert_field(value v);
CDecl convert_cdecl(value v);

std::string fdeclToString(const FDecl& f);
std::string cdeclToString(const CDecl& c);
