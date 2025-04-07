#include <caml/mlvalues.h>
#include <iostream>
#include <vector>
#include <string>
#include "mlvalues.h"
#include "ast/expr.hpp"
#include "ast/stmt.hpp"
#include "convert.hpp"
#include "ops.hpp"


// Function to convert OCaml blocks to C++ blocks
vector<Stmt*> convert_block(value ocaml_block) {
    vector<Stmt*> block;
    for (int i = 0; i < Wosize_val(ocaml_block); i++) {
        block.push_back(convert_stmt(Field(ocaml_block, i)));
    }
    return block;
}

Stmt* convert_stmt(value ocaml_stmt) {
    int tag = Tag_val(ocaml_stmt);
    switch (tag) {
        case 0: { // Assignment
            Expr* lhs = convert_expr(Field(ocaml_stmt, 0));
            AOp op = static_cast<AOp>(Int_val(Field(ocaml_stmt, 1)));
            Expr* rhs = convert_expr(Field(ocaml_stmt, 2));
            return new Assn(lhs, op, rhs);
        }
        case 1: // TODO var decl
            break;
        case 2: { // Return
            Expr* value = convert_expr(Field(ocaml_stmt, 0));
            return new ReturnStmt(value);
        }
        case 3: { // SCall
            Expr* func = convert_expr(Field(ocaml_stmt, 0));
            vector<Expr*> args;
            value arg_list = Field(ocaml_stmt, 1);
            for (int i = 0; i < Wosize_val(arg_list); i++) {
                args.push_back(convert_expr(Field(arg_list, i)));
            }
            return new StatementCall(func, args);
        }
        case 4: { // If statement
            Expr* cond = convert_expr(Field(ocaml_stmt, 0));
            vector<Stmt*> thenBlock = convert_block(Field(ocaml_stmt, 1));
            vector<Stmt*> elseBlock = convert_block(Field(ocaml_stmt, 2));
            return new IfStmt(cond, thenBlock, elseBlock);
        }
        case 5: { // For statement
            string var = String_val(Field(ocaml_stmt, 0));
            Expr* start = convert_expr(Field(ocaml_stmt, 1));
            Expr* end = convert_expr(Field(ocaml_stmt, 2));
            vector<Stmt*> body = convert_block(Field(ocaml_stmt, 3));
            return new ForStmt(var, start, end, body);
        }
        case 6: { // While statement
            Expr* cond = convert_expr(Field(ocaml_stmt, 0));
            vector<Stmt*> body = convert_block(Field(ocaml_stmt, 1));
            return new WhileStmt(cond, body);
        }
        case 7: { // Break statement
            return new BreakStmt();
        }
        case 8: { // Continue statement
            return new ContinueStmt();
        }
    }
}