#include <stdexcept>

#include <caml/mlvalues.h>

#include <ast/exp.h>


UnOp convert_unop(value v) {
    switch (Tag_val(v)) {
        case 0: return UnOp::Neg;
        case 1: return UnOp::Not;
        default: throw std::runtime_error("Unknown UnOp");
    }
}

BinOp convert_binop(value v) {
    switch (Tag_val(v)) {
        case 0: return BinOp::Add;
        case 1: return BinOp::Sub;
        case 2: return BinOp::Mul;
        case 3: return BinOp::Div;
        case 4: return BinOp::At;
        case 5: return BinOp::Mod;
        case 6: return BinOp::Pow;
        case 7: return BinOp::Eqeq;
        case 8: return BinOp::Neq;
        case 9: return BinOp::Lt;
        case 10: return BinOp::Lte;
        case 11: return BinOp::Gt;
        case 12: return BinOp::Gte;
        case 13: return BinOp::And;
        case 14: return BinOp::Or;
        default: throw std::runtime_error("Unknown BinOp");
    }
}

Node<Exp> convert_exp_node(value v) {
    Node<Exp> node;
    // get node loc
    value exp = Field(v, 0);

    if (Is_block(exp)) {
        switch(Tag_val(exp)){
            case 0: {
                bool b = Bool_val(Field(exp, 0));
                node.elt.val = EBool{b};
                break;
            }
            case 1: {
                int i = Int_val(Field(exp, 0));
                node.elt.val = EInt{i};
                break;
            }
            case 2: {
                double d = Double_val(Field(exp, 0));
                node.elt.val = EFloat{d};
                break;
            }
            case 3: {
                std::string s = String_val(Field(exp, 0));
                node.elt.val = EStr{s};
                break;
            }
            case 4: {
                value id_node = Field(exp, 0);
                std::string var = String_val(Field(id_node, 0));
                node.elt.val = EVar{var};
                break;
            }
            case 5: {
                Node<Exp> callee = convert_exp_node(Field(exp, 0));
                std::vector<std::unique_ptr<Node<Exp>>> args;
                value arg_list = Field(exp, 1);
                while (arg_list != Val_emptylist) {
                    value arg = Field(arg_list, 0);
                    args.push_back(std::make_unique<Node<Exp>>(convert_exp_node(arg)));
                    arg_list = Field(arg_list, 1);
                }
                node.elt.val = ECall {
                    .callee = std::make_unique<Node<Exp>>(std::move(callee)),
                    .args = std::move(args),
                };
                break;
            }
            case 6: {
                BinOp bop = static_cast<BinOp>(convert_binop(Int_val(Field(exp, 0))));
                Node<Exp> lhs = convert_exp_node(Field(exp, 1));
                Node<Exp> rhs = convert_exp_node(Field(exp, 2));
                node.elt.val = EBop {
                    .op = bop,
                    .left = std::make_unique<Node<Exp>>(std::move(lhs)),
                    .right = std::make_unique<Node<Exp>>(std::move(rhs)),
                };
                break;
            }
            case 7: {
                UnOp uop = static_cast<UnOp>(convert_unop(Int_val(Field(exp, 0))));
                Node<Exp> operand = convert_exp_node(Field(exp, 1));
                node.elt.val = EUop {
                    .op = uop,
                    .arg = std::make_unique<Node<Exp>>(std::move(operand)),
                };
                break;
            }
            case 8: {
                Node<Exp> collection = convert_exp_node(Field(exp, 0));
                Node<Exp> idx = convert_exp_node(Field(exp, 1));
                node.elt.val = EIndex {
                    .collection = std::make_unique<Node<Exp>>(std::move(collection)),
                    .index = std::make_unique<Node<Exp>>(std::move(idx)),
                };
                break;
            }
            case 9: {
                std::vector<std::unique_ptr<Node<Exp>>> elements;
                value elements_list = Field(exp, 0);
                while (elements_list != Val_emptylist) {
                    value element = Field(elements_list, 0);
                    elements.push_back(std::make_unique<Node<Exp>>(convert_exp_node(element)));
                    elements_list = Field(elements_list, 1);
                }
                node.elt.val = EArray {
                    .elements = std::move(elements),
                };
                break;
            }
            case 10: {
                Node<Exp> start = convert_exp_node(Field(exp, 0));
                Node<Exp> end = convert_exp_node(Field(exp, 1));
                bool inclusive = Bool_val(Field(exp, 2));
                node.elt.val = ERange {
                    .start = std::make_unique<Node<Exp>>(std::move(start)),
                    .end = std::make_unique<Node<Exp>>(std::move(end)),
                    .inclusive = inclusive,
                };
                break;
            }
        }
    }

    return node;
}
