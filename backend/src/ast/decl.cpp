#include <memory>
#include <string>
#include <variant>

#include <caml/mlvalues.h>

#include <ast/decl.h>

Node<GDecl> convert_gdecl_node(value v) {
    Node<GDecl> node;
    // get node loc
    value gdecl_val = Field(v, 0);

    GDecl g;
    g.name = std::string(String_val(Field(gdecl_val, 0)));
    g.init = convert_exp_node(Field(gdecl_val, 1));

    node.elt = std::move(g);
    return node;
}

Node<FDecl> convert_fdecl_node(value v) {
    Node<FDecl> node;
    // get node loc
    value fdecl_val = Field(v, 0);

    FDecl f;
    f.rtyp = convert_ret_ty(Field(fdecl_val, 0));
    f.fname = std::string(String_val(Field(fdecl_val, 1)));

    value args = Field(fdecl_val, 2);
    while (args != Val_emptylist) {
        value pair = Field(args, 0);
        f.args.emplace_back(
            convert_ty(Field(pair, 0)),
            std::string(String_val(Field(pair, 1)))
        );
        args = Field(args, 1);
    }

    value body = Field(fdecl_val, 3);
    while (body != Val_emptylist) {
        f.body.push_back(convert_stmt_node(Field(body, 0)));
        body = Field(body, 1);
    }

    node.elt = std::move(f);
    return node;
}

Decl convert_decl(value v) {
    Decl decl;
    int tag = Tag_val(v);

    switch (tag) {
        case 0: 
            decl.val = convert_gdecl_node(Field(v, 0));
            break;
        case 1:
            decl.val = convert_fdecl_node(Field(v, 0));
            break;
    }

    return decl;
}