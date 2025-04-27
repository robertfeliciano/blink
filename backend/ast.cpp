#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/hash.h>
#include <ast.h>

// Assumes you've included your structs above
Sint convert_sint(value v) {
    switch (Tag_val(v)) {
        case 0: return Sint::Ti8;
        case 1: return Sint::Ti16;
        case 2: return Sint::Ti32;
        case 3: return Sint::Ti64;
        case 4: return Sint::Ti128;
        default: throw std::runtime_error("Unknown Signed Int variant");
    }
}

Uint convert_uint(value v) {
    switch (Tag_val(v)) {
        case 0: return Uint::Tu8;
        case 1: return Uint::Tu16;
        case 2: return Uint::Tu32;
        case 3: return Uint::Tu64;
        case 4: return Uint::Tu128;
        default: throw std::runtime_error("Unknown Unsigned Int variant");
    }
}

FloatTy convert_float_ty(value v) {
    switch (Tag_val(v)) {
        case 0: return FloatTy::Tf32;
        case 1: return FloatTy::Tf64;
        default: throw std::runtime_error("Unknown FloatTy variant");
    }
}

IntTy convert_int_ty(value v) {
    IntTy result;
    if (Tag_val(v) == 0) {
        result.tag = IntTyTag::Signed;
        result.sint = convert_sint(Field(v, 0));
    } else {
        result.tag = IntTyTag::Unsigned;
        result.uint = convert_uint(Field(v, 0));
    }
    return result;
}

RefTy convert_ref_ty(value v) {
    RefTy ref;

    switch (Tag_val(v)) {
        case 0: { // RString
            ref.tag = RefTyTag::RString;
            break;
        }
        case 1: { // RArray of ty
            ref.tag = RefTyTag::RArray;
            ref.inner = std::make_unique<Ty>(convert_ty(Field(v, 0)));
            break;
        }
        case 2: { // RFun of ty list * ret_ty
            ref.tag = RefTyTag::RFun;
            value tys = Field(v, 0);
            while (tys != Val_emptylist) {
                value head = Field(tys, 0);
                ref.args.push_back(convert_ty(head));
                tys = Field(tys, 1);
            }
            ref.ret = convert_ret_ty(Field(v, 1));
            break;
        }
    }

    return ref;
}

Ty convert_ty(value v) {
    Ty ty;

    switch (Tag_val(v)) {
        case 0: // TBool
            ty.tag = TyTag::TBool;
            break;

        case 1: // TInt of int_ty
            ty.tag = TyTag::TInt;
            ty.int_ty = std::make_unique<IntTy>(convert_int_ty(Field(v, 0)));
            break;

        case 2: // TFloat of float_ty
            ty.tag = TyTag::TFloat;
            ty.float_ty = std::make_unique<FloatTy>(convert_float_ty(Field(v, 0)));
            break;

        case 3: // TRef of ref_ty
            ty.tag = TyTag::TRef;
            ty.ref_ty = std::make_unique<RefTy>(convert_ref_ty(Field(v, 0)));
            break;
    }

    return ty;
}

RetTy convert_ret_ty(value v) {
    RetTy ret;

    switch (Tag_val(v)) {
        case 0: // RetVoid
            ret.tag = RetTyTag::RetVoid;
            break;

        case 1: // RetVal of ty
            ret.tag = RetTyTag::RetVal;
            ret.val = std::make_unique<Ty>(convert_ty(Field(v, 0)));
            break;
    }

    return ret;
}

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
}

Node<Stmt> convert_stmt_node(value v) {
    Node<Stmt> node;
    // get node loc
    value stmt = Field(v, 0);

    if (Is_block(stmt)) {
        switch(Tag_val(stmt)){
            case 0: { 
                Node<Exp> lhs = convert_exp_node(Field(stmt, 0));
                AOp op = static_cast<AOp>(Int_val(Field(stmt, 1)));
                Node<Exp> rhs = convert_exp_node(Field(stmt, 2));
                node.elt.val = Assn {
                    .lhs = std::make_unique<Node<Exp>>(std::move(lhs)),
                    .op = op,
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
}

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
        default:
    }

    return decl;
}

Program convert_program(value v) {
    Program program;
    value decls = Field(v, 0);
    while (decls != Val_emptylist) {
        value decl = Field(decls, 0);
        program.push_back(convert_decl(decl));
        decls = Field(decls, 1);
    }
    return program;
}


