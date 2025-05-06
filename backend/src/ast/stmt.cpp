#include <utility>
#include <sstream>
#include <iostream>

#include <caml/mlvalues.h>

#include <ast/stmt.h>

Node<Stmt> convert_stmt_node(value v) {
    Node<Stmt> node;
    // get node loc
    value elt = Field(v, 0);

    if (Is_block(elt)) {
        switch(Tag_val(elt)){
            case 0: { // assn
                Node<Exp> lhs = convert_exp_node(Field(elt, 0));
                // ignore AOp since it will be desugared (x += 1 -> x = x + 1)
                Node<Exp> rhs = convert_exp_node(Field(elt, 2));
                node.elt.val = Assn {
                    .lhs = std::make_unique<Node<Exp>>(std::move(lhs)),
                    .rhs = std::make_unique<Node<Exp>>(std::move(rhs)),
                };
                break;
            }
            case 1: { // var decl
                value decl = Field(elt, 0);
                std::string id = String_val(Field(decl, 0));
                // at this point we know the types of all vdecls (after typechecking)
                value ty_option = Field(decl, 1);
                Ty ty = convert_ty(Field(ty_option, 0));
                Node<Exp> init = convert_exp_node(Field(decl, 2));
                bool is_const = Bool_val(Field(decl, 3));
                node.elt.val = VDecl {
                    .id = id,
                    .ty = std::move(ty),
                    .init = std::make_unique<Node<Exp>>(std::move(init)),
                    .is_const = is_const,
                };
                break;
            }
            case 2: { // ret
                value ret_val_option = Field(elt, 0);
                if (Is_block(ret_val_option)) {
                    value ret_exp_node = Field(ret_val_option, 0);
                    node.elt.val = Ret {
                        .value = std::make_shared<Node<Exp>>(convert_exp_node(ret_exp_node)),
                    };
                } else {
                    node.elt.val = Ret {
                        .value = std::nullopt,
                    };
                }
                break;
            }
            case 3: { // SCall
                Node<Exp> callee = convert_exp_node(Field(elt, 0));
                std::vector<std::unique_ptr<Node<Exp>>> args;
                value arg_list = Field(elt, 1);
                while (arg_list != Val_emptylist) {
                    value arg = Field(arg_list, 0);
                    args.push_back(std::make_unique<Node<Exp>>(convert_exp_node(arg)));
                    arg_list = Field(arg_list, 1);
                }
                node.elt.val = SCall {
                    .callee = std::make_unique<Node<Exp>>(std::move(callee)),
                    .args = std::move(args),
                };
                break;
            }
            case 4: { // if else
                Node<Exp> cond = convert_exp_node(Field(elt, 0));
                std::vector<std::unique_ptr<Node<Stmt>>> then_branch;
                value then_list = Field(elt, 1);
                while (then_list != Val_emptylist) {
                    value then_stmt = Field(then_list, 0);
                    then_branch.push_back(std::make_unique<Node<Stmt>>(convert_stmt_node(then_stmt)));
                    then_list = Field(then_list, 1);
                }
                std::vector<std::unique_ptr<Node<Stmt>>> else_branch;
                value else_list = Field(elt, 2);
                while (else_list != Val_emptylist) {
                    value else_stmt = Field(else_list, 0);
                    else_branch.push_back(std::make_unique<Node<Stmt>>(convert_stmt_node(else_stmt)));
                    else_list = Field(else_list, 1);
                }
                node.elt.val = If {
                    .cond = std::make_unique<Node<Exp>>(std::move(cond)),
                    .then_branch = std::move(then_branch),
                    .else_branch = std::move(else_branch),
                };
                break;
            }
            case 5: {
                // ignore for loop
            }
            case 6: { // while
                Node<Exp> cond = convert_exp_node(Field(elt, 0));
                std::vector<std::unique_ptr<Node<Stmt>>> body;
                value body_list = Field(elt, 1);
                while (body_list != Val_emptylist) {
                    value body_stmt_node = Field(body_list, 0);
                    body.push_back(std::make_unique<Node<Stmt>>(convert_stmt_node(body_stmt_node)));
                    body_list = Field(body_list, 1);
                }
                node.elt.val = While {
                    .cond = std::make_unique<Node<Exp>>(std::move(cond)),
                    .body = std::move(body),
                };
                break;
            }
        }
    } 
    else {
        switch(Int_val(elt)){
            case 0: {
                node.elt.val = Break{};
                break;
            }
            case 1: {
                node.elt.val = Continue{};
                break;
            }
        }
    }

    return node;
}

std::string indent(int level) {
    return std::string(level * 2, ' ');
}


struct StmtToStringVisitor {
    int indentLevel;
    StmtToStringVisitor(int level) : indentLevel(level) {}

    std::string operator()(const Assn& s) const {
        std::ostringstream oss;
        oss << indent(indentLevel) << expToString(s.lhs->elt) << " = " << expToString(s.rhs->elt) << ";";
        return oss.str();
    }
    std::string operator()(const Ret& ret) const {
        std::ostringstream oss;
        if (ret.value.has_value()) {
            std::shared_ptr<Node<Exp>> exp = ret.value.value();
            oss << indent(indentLevel) << "return " << expToString(exp->elt) << ";";
        } else {
            oss << indent(indentLevel) << "return;";
        }
        return oss.str();
    }
    std::string operator()(const SCall& s) const {
        std::ostringstream oss;
        oss << indent(indentLevel) << expToString(s.callee->elt) << "(";
        for (size_t i = 0; i < s.args.size(); ++i) {
            oss << expToString(s.args[i]->elt);
            if (i < s.args.size() - 1) {
                oss << ", ";
            }
        }
        oss << ");";
        return oss.str();
    }
    std::string operator()(const VDecl& s) const {
        std::ostringstream oss;
        // TODO create type to string function
        oss << indent(indentLevel) << (s.is_const ? "const " : "") << tyToString(s.ty) << " " << s.id;
        if (s.init) {
            oss << " = " << expToString(s.init->elt);
        }
        oss << ";";
        return oss.str();
    }
    std::string operator()(const If& s) const {
        std::ostringstream oss;
        oss << indent(indentLevel) << "if (" << expToString(s.cond->elt) << ") {\n";
        for (const auto& stmt : s.then_branch) {
            oss << stmtToString(stmt->elt, indentLevel + 1) << "\n";
        }
        oss << indent(indentLevel) << "}";
        if (!s.else_branch.empty()) {
            oss << " else {\n";
            for (const auto& stmt : s.else_branch) {
                oss << stmtToString(stmt->elt, indentLevel + 1) << "\n";
            }
            oss << indent(indentLevel) << "}";
        }
        return oss.str();
    }
    std::string operator()(const While& s) const {
        std::ostringstream oss;
        oss << indent(indentLevel) << "while (" << expToString(s.cond->elt) << ") {\n";
        for (const auto& stmt : s.body) {
            oss << stmtToString(stmt->elt, indentLevel + 1) << "\n";
        }
        oss << indent(indentLevel) << "}";
        return oss.str();
    }
    std::string operator()(const Break&) const {
        return indent(indentLevel) + "break;";
    }
    std::string operator()(const Continue&) const {
        return indent(indentLevel) + "continue;";
    }
};

std::string stmtToString(const Stmt& s, int indentLevel = 0) {
    return std::visit(StmtToStringVisitor{indentLevel}, s.val);
}