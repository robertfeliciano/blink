#include <iostream>
#include <vector>
#include "expr.hpp"
#include "stmt.hpp"

using namespace std;

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