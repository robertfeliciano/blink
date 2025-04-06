#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <iostream>
#include <vector>
#include <string>
using namespace std;


Ty* convert_ty(value ocaml_ty) {
    int tag = Tag_val(ocaml_ty);
    switch (tag) {
        case 0: // TBool
            return new TBool();
        case 1: { // TInt
            string intType = String_val(Field(ocaml_ty, 0)); // Assuming int_ty is represented as a string
            return new TInt(intType);
        }
        case 2: { // TFloat
            string floatType = String_val(Field(ocaml_ty, 0)); // Assuming float_ty is represented as a string
            return new TFloat(floatType);
        }
        case 3: { // TRef
            value ocaml_ref_ty = Field(ocaml_ty, 0);
            int ref_tag = Tag_val(ocaml_ref_ty);
            switch (ref_tag) {
                case 0: // RString
                    return new TRef(new TRef::RString());
                case 1: { // RArray
                    Ty* elementType = convert_ty(Field(ocaml_ref_ty, 0));
                    return new TRef(new TRef::RArray(elementType));
                }
                case 2: { // RFun
                    vector<Ty*> argTypes;
                    value ocaml_args = Field(ocaml_ref_ty, 0);
                    for (int i = 0; i < Wosize_val(ocaml_args); ++i) {
                        argTypes.push_back(convert_ty(Field(ocaml_args, i)));
                    }
                    Ty* returnType = convert_ty(Field(ocaml_ref_ty, 1));
                    return new TRef(new TRef::RFun(argTypes, returnType));
                }
                default:
                    throw runtime_error("Unknown ref_ty variant");
            }
        }
        default:
            throw runtime_error("Unknown ty variant");
    }
}

class Expr {
public:
    virtual void print() = 0;
    virtual ~Expr() {}
};

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

class FloatExpr : public Expr {
    float value;
public:
    FloatExpr(float v) : value(v) {}
    void print() override { cout << "Float: " << value << endl; }
};

class StrExpr : public Expr {
    string value;
public:
    StrExpr(string v) : value(v) {}
    void print() override { cout << "String: " << value << endl; }
};

class IdExpr : public Expr {
    string value;
public:
    IdExpr(string v) : value(v) {}
    void print() override { cout << "Id: " << value << endl; }
};

class CallExpr : public Expr {
    string func_name;
    vector<Expr*> args;
public: 
    CallExpr(string name, vector<Expr*> a) : func_name(name), args(a) {}
    void print() override {
        cout << "Function Call: " << func_name << "(";
        for (auto arg : args) {
            arg->print();
        }
        cout << ")" << endl;
    }
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

class UnOp : public Expr {
    UnOp op;
    Expr* operand;
public:
    UnOP(UnOp o, Expr* e) : op(o), operand(e) {}
    void print() override {
        cout << "UnaryOp: ";
        operand->print();
    }
};

class IndexExpr : public Expr {
    Expr* collection;
    Expr* idx;
public:
    IndexExpr(Expr* c, Expr* i) : collection(c), idx(i) {}
    void print() override {
        cout << "IndexOp: " << c->print() << "[" << i->print() << "]";
    }
};

class ListExpr : public Expr {
    vector<Expr*> elements;
public:
    ListExpr(vector<Expr*> elems) : elements(elems) {}
    void print() override {
        cout << "List: [";
        for (auto elem : elements) {
            elem->print();
        }
        cout << "]" << endl;
    }
};

class RangeExpr : public Expr {
    Expr* start;
    Expr* end;
    bool inclusive;
public: 
    RangeExpr(Expr* s, Expr* e, bool b) : start(s), end(e), inclusive(b) {}
    void print() override {
        cout << "Range: "; 
        start->print();
        cout << "..";
        if (inclusive) {
            cout << "=";
        }
        end->print();
    }
};

class Decl {
    string name;
    Expr* value;
    // TODO add type
    bool isConst;
public:
    Decl(string n, Expr* v, bool c) : name(n), value(v), isConst(c) {}
    void print() {
        if (isConst) {
            cout << "Const ";
        }
        cout << "Declaration: " << name << " = ";
        value->print();
    }
};

class Stmt {
public:
    virtual void print() = 0;
    virtual ~Stmt() {}
};

class Assn : public Stmt {
    Expr* lhs;
    AOp op;
    Expr* rhs;
public:
    Assn(Expr* l, AOp o, Expr* r) : lhs(l), op(o), rhs(r) {}
    void print() override {
        cout << "Assignment: ";
        lhs->print();
        cout << " " << op << " ";
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
        cout << "Then Block: ";
        for (auto stmt : thenBlock) {
            stmt->print();
        }
        cout << "Else Block: ";
        for (auto stmt : elseBlock) {
            stmt->print();
        }
    }
};

class WhileStmt : public Stmt {
    Expr* cond;
    vector<Stmt*> body;
public:
    WhileStmt(Expr* c, vector<Stmt*> b) : cond(c), body(b) {}
    void print() override {
        cout << "While Statement: ";
        cond->print();
        cout << "Body: ";
        for (auto stmt : body) {
            stmt->print();
        }
    }
};

class ForStmt : public Stmt {
    string var;
    Expr* start;
    Expr* end;
    vector<Stmt*> body;
public:
    ForStmt(string v, Expr* s, Expr* e, vector<Stmt*> b) : var(v), start(s), end(e), body(b) {}
    void print() override {
        cout << "For Statement: " << var << " in ";
        start->print();
        cout << " to ";
        end->print();
        cout << "Body: ";
        for (auto stmt : body) {
            stmt->print();
        }
    }
};

class StatementCall : public Stmt {
    Expr* func;
    vector<Expr*> args;
public: 
    StatementCall(Expr* f, vector<Expr*> a) : func(f), args(a) {}
    void print() override {
        cout << "Function Call: ";
        func->print();
        cout << "(";
        for (auto arg : args) {
            arg->print();
        }
        cout << ")" << endl;
    }
};

class ReturnStmt : public Stmt {
    Expr* value;
public:
    ReturnStmt(Expr* v) : value(v) {}
    void print() override {
        cout << "Return Statement: ";
        value->print();
    }
};

class ContinueStmt : public Stmt {
public:
    void print() override {
        cout << "Continue Statement" << endl;
    }
};

class BreakStmt : public Stmt {
public:
    void print() override {
        cout << "Break Statement" << endl;
    }
};

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

class FuncDecl {
    string name;
    string returnType; // Representing ret_ty as a string for simplicity TODO replace with type
    vector<pair<string, string>> args; // Pair of argument type and name
    vector<Stmt*> body; // Function body as a list of statements
public:
    FuncDecl(string n, string r, vector<pair<string, string>> a, vector<Stmt*> b)
        : name(n), returnType(r), args(a), body(b) {}

    void print() {
        cout << "Function Declaration: " << returnType << " " << name << "(";
        for (size_t i = 0; i < args.size(); ++i) {
            cout << args[i].first << " " << args[i].second;
            if (i < args.size() - 1) cout << ", ";
        }
        cout << ")" << endl;
        cout << "Body: " << endl;
        for (auto stmt : body) {
            stmt->print();
        }
    }
};

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
                return new RangeExpr(start, end);
            }
    }
}


extern "C" {
    value convert_caml_ast(value ocaml_ast) {
        CAMLparam1(ocaml_ast);
        Prog* prog = convert_ast(ocaml_ast);
        prog->print();
        delete ast;
        CAMLreturn(Val_unit);
    }
}