#include <stdexcept>
#include <iostream>
#include <sstream>

#include <caml/mlvalues.h>
#include <caml/custom.h>

#include <bridge/stmt.h>
#include <bridge/exp.h>
#include <bridge/types.h>
#include <util/constants.h>

static std::vector<std::unique_ptr<Stmt>> convert_block(value v) {
    std::vector<std::unique_ptr<Stmt>> out;
    while (v != Val_emptylist) {
        value head = Field(v, 0);
        out.push_back(std::make_unique<Stmt>(convert_stmt(head)));
        v = Field(v, 1);
    }
    return out;
}

Stmt convert_stmt(value v) {
    Stmt result;

    if (Is_block(v)) {
        switch (Tag_val(v)) {
            case Constants::STMT_Assn: { // Assn of exp * exp * ty
                auto lhs = std::make_unique<Exp>(convert_exp(Field(v, 0)));
                auto rhs = std::make_unique<Exp>(convert_exp(Field(v, 1)));
                Ty ty = convert_ty(Field(v, 2));
                result.val = Assn{ std::move(lhs), std::move(rhs), std::move(ty) };
                break;
            }
            case Constants::STMT_LambdaDecl: { // LambdaDecl of ldecl
                throw std::runtime_error("Lambda declarations not supported in bridge conversion");
            }
            case Constants::STMT_Decl: { // Decl of vdecl (id * ty * exp * bool)
                value vdecl = Field(v, 0);
                std::string id = String_val(Field(vdecl, 0));
                Ty ty = convert_ty(Field(vdecl, 1));
                auto init = std::make_unique<Exp>(convert_exp(Field(vdecl, 2)));
                bool is_const = Bool_val(Field(vdecl, 3));
                result.val = VDecl{ id, std::move(ty), std::move(init), is_const };
                break;
            }
            case Constants::STMT_Ret: { // Ret of exp option
                value opt = Field(v, 0);
                Ret r;
                if (Is_block(opt)) {
                    r.value = std::make_shared<Exp>(convert_exp(Field(opt, 0)));
                } else {
                    r.value = std::nullopt;
                }
                result.val = std::move(r);
                break;
            }
            case Constants::STMT_SCall: { // SCall of exp * exp list
                auto callee = std::make_unique<Exp>(convert_exp(Field(v, 0)));
                value args_v = Field(v, 1);
                std::vector<std::unique_ptr<Exp>> args;
                while (args_v != Val_emptylist) {
                    value head = Field(args_v, 0);
                    args.push_back(std::make_unique<Exp>(convert_exp(head)));
                    args_v = Field(args_v, 1);
                }
                result.val = SCall{ std::move(callee), std::move(args) };
                break;
            }
            case Constants::STMT_If: { // If of exp * block * block
                auto cond = std::make_unique<Exp>(convert_exp(Field(v, 0)));
                auto then_b = convert_block(Field(v, 1));
                auto else_b = convert_block(Field(v, 2));
                result.val = If{ std::move(cond), std::move(then_b), std::move(else_b) };
                break;
            }
            case Constants::STMT_While: { // While of exp * block
                auto cond = std::make_unique<Exp>(convert_exp(Field(v, 0)));
                auto body = convert_block(Field(v, 1));
                result.val = While{ std::move(cond), std::move(body) };
                break;
            }
            default: {
                throw std::runtime_error("Unsupported stmt variant in bridge conversion");
            }
        }
    }
    else {
        switch (Int_val(v)) {
            case Constants::CTRL_Break: { // Break
                result.val = Break{};
                break;
            }
            case Constants::CTRL_Continue: { // Continue
                result.val = Continue{};
                break;
            }
            default: {
                throw std::runtime_error("Unsupported stmt variant in bridge conversion");
            }
        }
    }

    return result;
}