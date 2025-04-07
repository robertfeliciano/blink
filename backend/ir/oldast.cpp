#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <iostream>
#include <vector>
#include <string>
using namespace std;

// Enum for binary operators
enum class BinOp { ADD, SUB, MUL, DIV, MOD, POW, EQEQ, NEQ, LT, GT, AND, OR };

// Enum for assignment operators
enum class AOp { EQ, PLUEQ, MINEQ, TIMEQ, DIVEQ, MODEQ, POWEQ };

// Base expression class
class Expr {
public:
    virtual void print() = 0;
    virtual ~Expr() {}
};

// Different expression types
class BoolExpr : public Expr {
    bool value;
public:
    BoolExpr(bool v) : value(v) {}
    void print() override { cout << "Bool: " << value << endl; }
};

class IntExpr : public Expr {
    int64_t value;
public:
    IntExpr(int64_t v) : value(v) {}
    void print() override { cout << "Int: " << value << endl; }
};

class BinOpExpr : public Expr {
    BinOp op;
    Expr* left;
    Expr* right;
public:
    BinOpExpr(BinOp o, Expr* l, Expr* r) : op(o), left(l), right(r) {}
    void print() override {
        cout << "BinaryOp: ";
        left->print();
        right->print();
    }
};

// Statement base class
class Stmt {
public:
    virtual void print() = 0;
    virtual ~Stmt() {}
};

class AssnStmt : public Stmt {
    Expr* lhs;
    AOp op;
    Expr* rhs;
public:
    AssnStmt(Expr* l, AOp o, Expr* r) : lhs(l), op(o), rhs(r) {}
    void print() override {
        cout << "Assignment Statement: ";
        lhs->print();
        rhs->print();
    }
};

class IfStmt : public Stmt {
    Expr* cond;
    vector<Stmt*> thenBlock;
    vector<Stmt*> elseBlock;
public:
    IfStmt(Expr* c, vector<Stmt*> t, vector<Stmt*> e) : cond(c), thenBlock(t), elseBlock(e) {}
    void print() override {
        cout << "If Statement: ";
        cond->print();
    }
};

// Function to convert OCaml binary operator to C++ BinOp
typedef value ocaml_binop;
BinOp convert_binop(ocaml_binop op) {
    switch (Int_val(op)) {
        case 0: return BinOp::ADD;
        case 1: return BinOp::SUB;
        case 2: return BinOp::MUL;
        case 3: return BinOp::DIV;
        case 4: return BinOp::MOD;
        case 5: return BinOp::POW;
        case 6: return BinOp::EQEQ;
        case 7: return BinOp::NEQ;
        case 8: return BinOp::LT;
        case 9: return BinOp::GT;
        case 10: return BinOp::AND;
        case 11: return BinOp::OR;
        default: throw runtime_error("Unknown binary operator");
    }
}

// Function to convert OCaml assignment operator to C++ AOp
typedef value ocaml_aop;
AOp convert_aop(ocaml_aop op) {
    switch (Int_val(op)) {
        case 0: return AOp::EQ;
        case 1: return AOp::PLUEQ;
        case 2: return AOp::MINEQ;
        case 3: return AOp::TIMEQ;
        case 4: return AOp::DIVEQ;
        case 5: return AOp::MODEQ;
        case 6: return AOp::POWEQ;
        default: throw runtime_error("Unknown assignment operator");
    }
}

// Function to convert OCaml AST to C++ AST (for expressions only)
Expr* convert_ast(value ocaml_expr) {
    if (Is_long(ocaml_expr)) {
        return new IntExpr(Long_val(ocaml_expr));
    }
    int tag = Tag_val(ocaml_expr);
    switch (tag) {
        case 0: // Bool
            return new BoolExpr(Bool_val(Field(ocaml_expr, 0)));
        case 1: // Int
            return new IntExpr(Int64_val(Field(ocaml_expr, 0)));
        case 2: { // BinOp
            BinOp op = convert_binop(Field(ocaml_expr, 0));
            Expr* left = convert_ast(Field(ocaml_expr, 1));
            Expr* right = convert_ast(Field(ocaml_expr, 2));
            return new BinOpExpr(op, left, right);
        }
        default:
            throw runtime_error("Unknown AST node type for expression");
    }
}

// Function to convert OCaml AST statements to C++ AST
Stmt* convert_stmt(value ocaml_stmt) {
    int tag = Tag_val(ocaml_stmt);
    switch (tag) {
        case 0: { // Assignment
            Expr* lhs = convert_ast(Field(ocaml_stmt, 0));
            AOp op = convert_aop(Field(ocaml_stmt, 1));
            Expr* rhs = convert_ast(Field(ocaml_stmt, 2));
            return new AssnStmt(lhs, op, rhs);
        }
        case 1: { // If statement
            Expr* cond = convert_ast(Field(ocaml_stmt, 0));
            vector<Stmt*> thenBlock = convert_block(Field(ocaml_stmt, 1));
            vector<Stmt*> elseBlock = convert_block(Field(ocaml_stmt, 2));
            return new IfStmt(cond, thenBlock, elseBlock);
        }
        default:
            throw runtime_error("Unknown AST node type for statement");
    }
}


extern "C" {
    value caml_convert_ast(value ocaml_expr) {
        CAMLparam1(ocaml_expr);
        Expr* ast = convert_ast(ocaml_expr);
        ast->print();
        delete ast;
        CAMLreturn(Val_unit);
    }
}
