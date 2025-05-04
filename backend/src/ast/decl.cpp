#include <memory>
#include <string>
#include <sstream>

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

Node<FDecl> convert_fdecl_node(value fdecl_node) {
    Node<FDecl> node;
    value elt = Field(fdecl_node, 0);
    // get node loc... value loc = Field(fdecl, 1)
    FDecl f;
    f.rtyp = convert_ret_ty(Field(elt, 0));
    f.fname = std::string(String_val(Field(elt, 1)));

    value args = Field(elt, 2);
    while (args != Val_emptylist) {
        value pair = Field(args, 0);
        f.args.emplace_back(
            convert_ty(Field(pair, 0)),
            String_val(Field(pair, 1))
        );
        args = Field(args, 1);
    }

    value body = Field(elt, 3);
    while (body != Val_emptylist) {
        value stmt_node = Field(body, 0);
        f.body.push_back(convert_stmt_node(stmt_node));
        body = Field(body, 1);
    }
    node.elt = std::move(f);
    return node;
}

Decl convert_decl(value d) {
    Decl decl;

    switch (Tag_val(d)) {
        case 0:
            value fdecl_node = Field(d, 0);
            decl.val = convert_fdecl_node(fdecl_node);
            break;
    }

    return decl;
}

struct DeclToStringVisitor {
    std::string operator()(const Node<GDecl>& gNode) const {
        const auto &g = gNode.elt;
        return "global " + g.name + " = " + expToString(g.init.elt) + ";";
    }
    std::string operator()(const Node<FDecl>& fNode) const {
        const auto &f = fNode.elt;
        std::ostringstream oss;
        oss << "fun " << f.fname << "(";
        for (size_t i = 0; i < f.args.size(); ++i) {
            oss << tyToString(f.args[i].first) << " " << f.args[i].second;
            if (i < f.args.size() - 1) {
                oss << ", ";
            }
        }
        switch (f.rtyp.tag) {
            case RetTyTag::RetVoid:
                oss << ") -> void";
                break;
            case RetTyTag::RetVal:
                oss << ") -> " << tyToString(*f.rtyp.val);
                break;
        }
        oss << " {\n";
        for (const auto& stmt : f.body) {
            oss << stmtToString(stmt.elt, 1) << "\n";
        }
        oss << "}";
        return oss.str();
    }
};

std::string declToString(const Decl& decl) {
    return std::visit(DeclToStringVisitor{}, decl.val);
}