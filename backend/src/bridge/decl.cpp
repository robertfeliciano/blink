#include <bridge/decl.h>
#include <bridge/exp.h>
#include <bridge/stmt.h>
#include <bridge/types.h>
#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <iostream>
#include <sstream>
#include <stdexcept>

static std::vector<std::unique_ptr<Stmt>> convert_block(value v) {
    std::vector<std::unique_ptr<Stmt>> out;
    while (v != Val_emptylist) {
        value head = Field(v, 0);
        out.push_back(std::make_unique<Stmt>(convert_stmt(head)));
        v = Field(v, 1);
    }
    return out;
}

FDecl convert_fdecl(value v) { // record fdecl = { frtyp; fname; args; mutable body }
    FDecl out;

    out.frtyp = convert_ret_ty(Field(v, 0));
    out.fname = std::string(String_val(Field(v, 1)));

    // args : (ty * id) list
    value args_v = Field(v, 2);
    while (args_v != Val_emptylist) {
        value       pair = Field(args_v, 0);
        Ty          t    = convert_ty(Field(pair, 0));
        std::string id   = std::string(String_val(Field(pair, 1)));
        out.args.emplace_back(std::move(t), std::move(id));
        args_v = Field(args_v, 1);
    }

    // body : stmt list
    value body_v = Field(v, 3);
    while (body_v != Val_emptylist) {
        value hd = Field(body_v, 0);
        out.body.push_back(std::make_unique<Stmt>(convert_stmt(hd)));
        body_v = Field(body_v, 1);
    }

    return out;
}

Field convert_field(value v) { // record field = { prelude : stmt list; fieldName : id; ftyp : ty; init : exp }
    Field out;
    out.fieldName = std::string(String_val(Field(v, 1)));
    out.ftyp      = convert_ty(Field(v, 2));
    out.init      = std::make_unique<Exp>(convert_exp(Field(v, 3)));

    value prelude_v = Field(v, 0);
    while (prelude_v != Val_emptylist) {
        value head = Field(prelude_v, 0);
        out.prelude.push_back(std::make_unique<Stmt>(convert_stmt(head)));
        prelude_v = Field(prelude_v, 1);
    }

    return out;
}

CDecl convert_cdecl(value v) { // record cdecl = { cname : id; fields : field list }
    CDecl out;
    out.cname      = std::string(String_val(Field(v, 0)));
    value fields_v = Field(v, 1);
    while (fields_v != Val_emptylist) {
        value head = Field(fields_v, 0);
        out.fields.push_back(convert_field(head));
        fields_v = Field(fields_v, 1);
    }

    return out;
}