#pragma once

#include <string>
#include <vector>
#include <variant>
#include <sstream>
#include <string_view>
#include <iostream>
#include <optional>
#include <memory>

enum class Sint {
    Ti8, Ti16, Ti32, Ti64, Ti128
};

enum class Uint {
    Tu8, Tu16, Tu32, Tu64, Tu128
};

enum class FloatTy {
    Tf32, Tf64
};

enum class IntTyTag { Signed, Unsigned };

struct IntTy {
    IntTyTag tag;
    union {
        Sint sint;
        Uint uint;
    };
};

enum class RefTyTag { RString, RArray, RFun };

struct RefTy {
    RefTyTag tag;
    std::vector<Ty> args;         // for RFun
    RetTy ret;                    // for RFun
    std::unique_ptr<Ty> inner;    // for RArray
};

enum class TyTag {
    TBool,
    TInt,
    TFloat,
    TRef
};

struct Ty {
    TyTag tag;
    std::unique_ptr<IntTy> int_ty;
    std::unique_ptr<FloatTy> float_ty;
    std::unique_ptr<RefTy> ref_ty;
};

enum class RetTyTag {
    RetVoid,
    RetVal
};

struct RetTy {
    RetTyTag tag;
    std::unique_ptr<Ty> val;
};

enum class UnOp {
    Neg,
    Not
};

enum class BinOp {
    Add,
    Sub,
    Mul,
    Div,
    At,
    Mod,
    Pow,
    Eqeq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or
};

enum class AOp {
    Eq,
    PluEq,
    MinEq,
    TimEq,
    DivEq,
    AtEq,
    PowEq,
    ModEq
};

template<typename T>
struct Node {
    T elt;
    int loc;
};


struct EInt  { int value; };
struct EBool { bool value; };
struct EVar  { std::string id; };
struct EFloat { double value; };
struct EStr { std::string value; };
struct ECall { 
    std::unique_ptr<Node<Exp>> callee;
    std::vector<std::unique_ptr<Node<Exp>>> args;
};
struct EBop { 
    BinOp op;
    std::unique_ptr<Node<Exp>> left;
    std::unique_ptr<Node<Exp>> right;
};
struct EUop { 
    UnOp op;
    std::unique_ptr<Node<Exp>> arg;
};
struct EIndex {
    std::unique_ptr<Node<Exp>> collection;
    std::unique_ptr<Node<Exp>> index;
};
struct EArray {
    std::vector<std::unique_ptr<Node<Exp>>> elements;
};
struct ERange {
    std::unique_ptr<Node<Exp>> start;
    std::unique_ptr<Node<Exp>> end;
    bool inclusive;
};

using ExpVariant = std::variant<
    EInt, 
    EBool, 
    EVar,
    EFloat,
    EStr,
    ECall,
    EBop,
    EUop,
    EIndex,
    EArray,
    ERange>;

struct Exp {
    ExpVariant val;
};

struct Assn {
    std::unique_ptr<Node<Exp>> lhs;
    AOp op;
    std::unique_ptr<Node<Exp>> rhs;
};

struct VDecl {
    std::string id;
    Ty ty;
    std::unique_ptr<Node<Exp>> init;
    bool is_const;
};

struct Ret {
    std::optional<std::unique_ptr<Node<Exp>>> value;
};

struct SCall {
    std::unique_ptr<Node<Exp>> callee;
    std::vector<std::unique_ptr<Node<Exp>>> args;
};

struct If {
    std::unique_ptr<Node<Exp>> cond;
    std::vector<std::unique_ptr<Node<Stmt>>> then_branch;
    std::vector<std::unique_ptr<Node<Stmt>>> else_branch;
};

// do not need a For struct since desugaring will make For into While

struct While {
    std::unique_ptr<Node<Exp>> cond;
    std::vector<std::unique_ptr<Node<Stmt>>> body;
};

struct Break {};

struct Continue {};

using StmtVariant = std::variant<
    Assn,
    VDecl,
    Ret,
    SCall,
    If,
    While,
    Break,
    Continue>;

struct Stmt {
    StmtVariant val;
};

struct GDecl {
    std::string name;
    Node<Exp> init;
};

struct FDecl {
    RetTy rtyp;
    std::string fname;
    std::vector<std::pair<Ty, std::string>> args;
    std::vector<Node<Stmt>> body;
};

using DeclVariant = std::variant<Node<GDecl>, Node<FDecl>>;

struct Decl {
    DeclVariant val;
};

using Program = std::vector<Decl>;