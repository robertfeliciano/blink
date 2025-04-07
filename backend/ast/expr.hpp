#include <iostream>
#include <vector>
#include "ops.hpp"

using namespace std;

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

class UnOpExpr : public Expr {
    UnOp op;
    Expr* operand;
public:
    UnOpExpr(UnOp o, Expr* e) : op(o), operand(e) {}
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
        cout << "IndexOp: ";
        collection->print();
        cout  << "[";
        idx->print();
        cout << "]";
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