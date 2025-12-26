#pragma once

#include <bridge/exp.h>
#include <bridge/stmt.h>
#include <bridge/types.h>
#include <caml/mlvalues.h>
#include <memory>
#include <string>
#include <vector>

struct FDecl {
    RetTy                                   frtyp;
    std::string                             fname;
    std::vector<std::pair<Ty, std::string>> args;
    std::vector<std::unique_ptr<Stmt>>      body;
    std::vector<std::string>                annotations;
};

struct Field {
    std::vector<std::unique_ptr<Stmt>> prelude;
    std::string                        fieldName;
    Ty                                 ftyp;
    std::unique_ptr<Exp>               init;
};

struct CDecl {
    std::string              cname;
    std::vector<std::string> impls;
    std::vector<Field>       fields;
    std::vector<std::string> annotations;
};

struct Proto {
    std::vector<std::string> annotations;
    RetTy                    frtyp;
    std::string              fname;
    std::vector<Ty>          args;
};

FDecl convert_fdecl(value v);
Field convert_field(value v);
CDecl convert_cdecl(value v);
Proto convert_proto(value v);
