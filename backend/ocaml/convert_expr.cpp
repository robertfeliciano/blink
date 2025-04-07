#include <iostream>
#include <vector>
#include <string>
#include "convert.hpp"
#include "ast/expr.hpp"
#include "ast/ops.hpp"

Expr* convert_expr(value ocaml_exp) {
    int tag = Tag_val(ocaml_exp);
    switch (tag) {
        case 0:
            return new BoolExpr(Bool_val(Field(ocaml_exp, 0)));
        case 1: 
            return new IntExpr(Int64_val(Field(ocaml_exp, 0)));
        case 2: 
            return new FloatExpr(Double_val(Field(ocaml_exp, 0)));
        case 3: 
            return new StrExpr(String_val(Field(ocaml_exp, 0)));
        case 4: 
            return new IdExpr(String_val(Field(ocaml_exp, 0)));
        case 5: 
            {
                string func_name = String_val(Field(ocaml_exp, 0));
                vector<Expr*> args;
                value arg_list = Field(ocaml_exp, 1);
                for (int i = 0; i < Wosize_val(arg_list); i++) {
                    args.push_back(convert_expr(Field(arg_list, i)));
                }
                return new CallExpr(func_name, args);
            }
        case 6: 
            {
                BinOp op = static_cast<BinOp>(Int_val(Field(ocaml_exp, 0)));
                Expr* left = convert_expr(Field(ocaml_exp, 1));
                Expr* right = convert_expr(Field(ocaml_exp, 2));
                return new BinOpExpr(op, left, right);
            }
        case 7: 
            {
                UnOp op = static_cast<UnOp>(Int_val(Field(ocaml_exp, 0)));
                Expr* operand = convert_expr(Field(ocaml_exp, 1));
                return new UnOpExpr(op, operand);
            }
        case 8: 
            {
                Expr* collection = convert_expr(Field(ocaml_exp, 0));
                Expr* idx = convert_expr(Field(ocaml_exp, 1));
                return new IndexExpr(collection, idx);
            }
        case 9: 
            {
                vector<Expr*> elements;
                value elem_list = Field(ocaml_exp, 0);
                for (int i = 0; i < Wosize_val(elem_list); i++) {
                    elements.push_back(convert_expr(Field(elem_list, i)));
                }
                return new ListExpr(elements);
            }
        case 10: 
            {
                Expr* start = convert_expr(Field(ocaml_exp, 0));
                Expr* end = convert_expr(Field(ocaml_exp, 1));
                bool inc = Bool_val(Field(ocaml_exp, 2));
                return new RangeExpr(start, end, inc);
            }
    }
}
