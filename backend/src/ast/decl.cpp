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

struct DeclToStringVisitor {
    std::string operator()(const Node<GDecl>& gNode) const {
        const auto &g = gNode.elt;
        return "global " + g.name + " = " + expToString(g.init.elt) + ";";
    }
    std::string operator()(const Node<FDecl>& fNode) const {
        const auto &f = fNode.elt;
        std::ostringstream oss;
        oss << "function " << f.fname << "(";
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
        for (const auto& stmt : f.body) {
            oss << stmtToString(stmt.elt, 1) << "\n";
        }
        oss << "}";
        return oss.str();
    }
};

inline std::string declToString(const Decl& decl) {
    return std::visit(DeclToStringVisitor{}, decl.val);
}