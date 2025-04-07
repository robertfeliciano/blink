#include <vector>
#include <string>
#include "ops.hpp"
#include "expr.hpp"
using namespace std;



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
        cout << " ";
        switch (op) {
            case AOp::EQ: cout << "="; break;
            case AOp::PLUEQ: cout << "+="; break;
            case AOp::MINEQ: cout << "-="; break;
            case AOp::TIMEQ: cout << "*="; break;
            case AOp::DIVEQ: cout << "/="; break;
            case AOp::ATEQ: cout << "@="; break;
            case AOp::POWEQ: cout << "^="; break;
            case AOp::MODEQ: cout << "%="; break;
        }
        cout << " ";
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