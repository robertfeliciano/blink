#include <utility>

#include <caml/mlvalues.h>

#include <ast/stmt.h>

Node<Stmt> convert_stmt_node(value v) {
    Node<Stmt> node;
    // get node loc
    value stmt = Field(v, 0);

    if (Is_block(stmt)) {
        switch(Tag_val(stmt)){
            case 0: { 
                Node<Exp> lhs = convert_exp_node(Field(stmt, 0));
                // ignore AOp since it will be desugared (x += 1 -> x = x + 1)
                Node<Exp> rhs = convert_exp_node(Field(stmt, 2));
                node.elt.val = Assn {
                    .lhs = std::make_unique<Node<Exp>>(std::move(lhs)),
                    .rhs = std::make_unique<Node<Exp>>(std::move(rhs)),
                };
                break;
            }
            case 1: { 
                value decl = Field(stmt, 0);
                std::string id = String_val(Field(decl, 0));
                // at this point we know the types of all vdecls (after typechecking)
                Ty ty = convert_ty(Field(decl, 1));
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
            case 2: { 
                value ret_val = Field(stmt, 0);
                if (Is_block(ret_val)) {
                    node.elt.val = Ret {
                        .value = std::make_unique<Node<Exp>>(convert_exp_node(Field(ret_val, 0))),
                    };
                } else {
                    node.elt.val = Ret {
                        .value = std::nullopt,
                    };
                }
                break;
            }
            case 3: { 
                Node<Exp> callee = convert_exp_node(Field(stmt, 0));
                std::vector<std::unique_ptr<Node<Exp>>> args;
                value arg_list = Field(stmt, 1);
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
            case 4: { 
                Node<Exp> cond = convert_exp_node(Field(stmt, 0));
                std::vector<std::unique_ptr<Node<Stmt>>> then_branch;
                value then_list = Field(stmt, 1);
                while (then_list != Val_emptylist) {
                    value then_stmt = Field(then_list, 0);
                    then_branch.push_back(std::make_unique<Node<Stmt>>(convert_stmt_node(then_stmt)));
                    then_list = Field(then_list, 1);
                }
                std::vector<std::unique_ptr<Node<Stmt>>> else_branch;
                value else_list = Field(stmt, 2);
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
                Node<Exp> cond = convert_exp_node(Field(stmt, 0));
                std::vector<std::unique_ptr<Node<Stmt>>> body;
                value body_list = Field(stmt, 1);
                while (body_list != Val_emptylist) {
                    value body_stmt = Field(body_list, 0);
                    body.push_back(std::make_unique<Node<Stmt>>(convert_stmt_node(body_stmt)));
                    body_list = Field(body_list, 1);
                }
                node.elt.val = While {
                    .cond = std::make_unique<Node<Exp>>(std::move(cond)),
                    .body = std::move(body),
                };
                break;
            }
        }
    } else {
        switch(Tag_val(stmt)){
            case 6: { 
                node.elt.val = Break{};
                break;
            }
            case 7: { 
                node.elt.val = Continue{};
                break;
            }
        }
    }

    return node;
}
