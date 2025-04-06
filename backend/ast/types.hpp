#pragma once
#include <string>
#include <iostream>
#include <vector>

using namespace std;

// Base class for types
class Ty {
public:
    virtual void print() = 0;
    virtual ~Ty() {}
};

// Boolean type
class TBool : public Ty {
public:
    void print() override {
        cout << "TBool" << endl;
    }
};

// Integer type
class TInt : public Ty {
    string intType; // Representing int_ty as a string for simplicity
public:
    TInt(string t) : intType(t) {}
    void print() override {
        cout << "TInt: " << intType << endl;
    }
};

// Float type
class TFloat : public Ty {
    string floatType; // Representing float_ty as a string for simplicity
public:
    TFloat(string t) : floatType(t) {}
    void print() override {
        cout << "TFloat: " << floatType << endl;
    }
};

// Reference type
class TRef : public Ty {
public:
    class RefTy {
    public:
        virtual void print() = 0;
        virtual ~RefTy() {}
    };

    class RString : public RefTy {
    public:
        void print() override {
            cout << "RString" << endl;
        }
    };

    class RArray : public RefTy {
        Ty* elementType;
    public:
        RArray(Ty* t) : elementType(t) {}
        void print() override {
            cout << "RArray of ";
            elementType->print();
        }
    };

    class RFun : public RefTy {
        vector<Ty*> argTypes;
        Ty* returnType;
    public:
        RFun(vector<Ty*> args, Ty* ret) : argTypes(args), returnType(ret) {}
        void print() override {
            cout << "RFun: (";
            for (size_t i = 0; i < argTypes.size(); ++i) {
                argTypes[i]->print();
                if (i < argTypes.size() - 1) cout << ", ";
            }
            cout << ") -> ";
            returnType->print();
        }
    };

    RefTy* refType;
    TRef(RefTy* r) : refType(r) {}
    void print() override {
        cout << "TRef: ";
        refType->print();
    }
};

// Return type
class RetTy {
public:
    virtual void print() = 0;
    virtual ~RetTy() {}
};

class RetVoid : public RetTy {
public:
    void print() override {
        cout << "RetVoid" << endl;
    }
};

class RetVal : public RetTy {
    Ty* valueType;
public:
    RetVal(Ty* t) : valueType(t) {}
    void print() override {
        cout << "RetVal of ";
        valueType->print();
    }
};