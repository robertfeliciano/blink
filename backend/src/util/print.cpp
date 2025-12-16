#include <algorithm>
#include <bridge/decl.h>
#include <bridge/exp.h>
#include <bridge/prog.h>
#include <bridge/stmt.h>
#include <bridge/types.h>
#include <cstdint>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <util/print.h>
#include <vector>

using i128 = __int128;
using u128 = unsigned __int128;

static std::string i128_to_string(i128 v) {
    if (v == 0)
        return "0";
    bool        neg = v < 0;
    u128        uv  = neg ? static_cast<u128>(-v) : static_cast<u128>(v);
    std::string s;
    while (uv > 0) {
        int digit = static_cast<int>(uv % 10);
        s.push_back('0' + digit);
        uv /= 10;
    }
    if (s.empty())
        s = "0";
    std::reverse(s.begin(), s.end());
    if (neg)
        s.insert(s.begin(), '-');
    return s;
}

static std::string u128_to_string(u128 v) {
    if (v == 0)
        return "0";
    std::string s;
    while (v > 0) {
        int digit = static_cast<int>(v % 10);
        s.push_back('0' + digit);
        v /= 10;
    }
    if (s.empty())
        s = "0";
    std::reverse(s.begin(), s.end());
    return s;
}

static std::string indent(int level) {
    return std::string(level * 4, ' ');
}

std::string tyToString(const Ty& ty) {
    switch (ty.tag) {
        case TyTag::TBool:
            return "bool";
        case TyTag::TInt: {
            const IntTy& it = *ty.int_ty;
            if (it.tag == IntTyTag::Signed) {
                switch (it.sint) {
                    case Sint::Ti8:
                        return "i8";
                    case Sint::Ti16:
                        return "i16";
                    case Sint::Ti32:
                        return "i32";
                    case Sint::Ti64:
                        return "i64";
                    case Sint::Ti128:
                        return "i128";
                }
            } else {
                switch (it.uint) {
                    case Uint::Tu8:
                        return "u8";
                    case Uint::Tu16:
                        return "u16";
                    case Uint::Tu32:
                        return "u32";
                    case Uint::Tu64:
                        return "u64";
                    case Uint::Tu128:
                        return "u128";
                }
            }
            break;
        }
        case TyTag::TFloat: {
            switch (*ty.float_ty) {
                case FloatTy::Tf32:
                    return "f32";
                case FloatTy::Tf64:
                    return "f64";
            }
            break;
        }
        case TyTag::TRef: {
            const RefTy& r = *ty.ref_ty;
            switch (r.tag) {
                case RefTyTag::RString:
                    return "string";
                case RefTyTag::RArray: {
                    return "[" + std::to_string(r.size) + " x " + tyToString(*r.inner) + "]";
                    // return tyToString(*r.inner) + "[" + std::to_string(r.size) + "]";
                }
                case RefTyTag::RClass:
                    return r.cname;
                case RefTyTag::RFun: {
                    std::string s = "(";
                    for (size_t i = 0; i < r.args.size(); ++i) {
                        s += tyToString(r.args[i]);
                        if (i + 1 < r.args.size())
                            s += ", ";
                    }
                    s += ") -> ";
                    if (r.ret.tag == RetTyTag::RetVoid)
                        s += "void";
                    else
                        s += tyToString(*r.ret.val);
                    return s;
                }
            }
            break;
        }
    }
    return "<unknown>";
}

std::string toString(BinOp op) {
    switch (op) {
        case BinOp::Add:
            return "+";
        case BinOp::Sub:
            return "-";
        case BinOp::Mul:
            return "*";
        case BinOp::Div:
            return "/";
        case BinOp::At:
            return "@";
        case BinOp::Mod:
            return "%";
        case BinOp::Pow:
            return "**";
        case BinOp::Eqeq:
            return "==";
        case BinOp::Neq:
            return "!=";
        case BinOp::Lt:
            return "<";
        case BinOp::Lte:
            return "<=";
        case BinOp::Gt:
            return ">";
        case BinOp::Gte:
            return ">=";
        case BinOp::And:
            return "&&";
        case BinOp::Or:
            return "||";
        case BinOp::Shl:
            return "<<";
        case BinOp::Lshr:
            return ">>";
        case BinOp::Ashr:
            return ">>>";
        case BinOp::Xor:
            return "xor";
        case BinOp::BXor:
            return "^";
        case BinOp::BAnd:
            return "&";
        case BinOp::BOr:
            return "|";
    }
    throw std::runtime_error("Unknown BinOp");
}

std::string toString(UnOp op) {
    switch (op) {
        case UnOp::Neg:
            return "-";
        case UnOp::Not:
            return "!";
        case UnOp::BNeg:
            return "~";
    }
    throw std::runtime_error("Unknown UnOp");
}

struct ExpToStringVisitor {
    std::string operator()(const EBool& e) const { return e.value ? "true" : "false"; }
    std::string operator()(const EInt& e) const {
        if (e.int_ty->tag == IntTyTag::Signed)
            return i128_to_string(e.s);
        else
            return u128_to_string(e.u);
    }
    std::string operator()(const EFloat& e) const { return std::to_string(e.value); }
    std::string operator()(const EStr& e) const { return "\"" + e.value + "\""; }
    std::string operator()(const EId& e) const { return e.id; }
    std::string operator()(const ECall& e) const {
        std::string res = expToString(*e.callee) + "(";
        for (size_t i = 0; i < e.args.size(); ++i) {
            res += expToString(*e.args[i]);
            if (i + 1 < e.args.size())
                res += ", ";
        }
        res += ")";
        return res;
    }
    std::string operator()(const EBop& e) const {
        return "(" + expToString(*e.left) + " " + toString(e.op) + " " + expToString(*e.right) + ")";
    }
    std::string operator()(const EUop& e) const { return "(" + toString(e.op) + " " + expToString(*e.arg) + ")"; }
    std::string operator()(const EIndex& e) const {
        return expToString(*e.collection) + "[" + expToString(*e.index) + "]";
    }
    std::string operator()(const EArray& e) const {
        std::string res = "[";
        for (size_t i = 0; i < e.elements.size(); ++i) {
            res += expToString(*e.elements[i]);
            if (i + 1 < e.elements.size())
                res += ", ";
        }
        res += "]";
        return res;
    }
    std::string operator()(const ECast& e) const {
        return "(" + expToString(*e.expr) + " as " + tyToString(e.ty) + ")";
    }
    std::string operator()(const EProj& e) const { return expToString(*e.obj) + "." + e.field; }
    std::string operator()(const EObjInit& e) const {
        std::string res = e.id + "{";
        for (size_t i = 0; i < e.fields.size(); ++i) {
            res += e.fields[i].first + ":= " + expToString(*e.fields[i].second);
            if (i + 1 < e.fields.size())
                res += ", ";
        }
        res += "}";
        return res;
    }
    std::string operator()(const ENull& e) const { return "nullptr to " + tyToString(e.ty); }
};

std::string expToString(const Exp& exp) {
    return std::visit(ExpToStringVisitor{}, exp.val);
}

struct StmtToStringVisitor {
    int indentLevel;

    std::string operator()(const Assn& s) const {
        return indent(indentLevel) + expToString(*s.lhs) + " = " + expToString(*s.rhs) + ";";
    }
    std::string operator()(const VDecl& s) const {
        return indent(indentLevel) + (s.is_const ? "const " : "let ") + s.id + ": " + tyToString(s.ty) + " = " +
               expToString(*s.init) + ";";
    }
    std::string operator()(const Ret& s) const {
        if (s.value.has_value())
            return indent(indentLevel) + "return " + expToString(*s.value.value()) + ";";
        else
            return indent(indentLevel) + "return;";
    }
    std::string operator()(const SCall& s) const {
        std::string res = indent(indentLevel) + expToString(*s.callee) + "(";
        for (size_t i = 0; i < s.args.size(); ++i) {
            res += expToString(*s.args[i]);
            if (i + 1 < s.args.size())
                res += ", ";
        }
        res += ");";
        return res;
    }
    std::string operator()(const If& s) const {
        std::string res = indent(indentLevel) + "if (" + expToString(*s.cond) + ") {\n";
        for (auto& st : s.then_branch)
            res += stmtToString(*st, indentLevel + 1) + "\n";
        res += indent(indentLevel) + " } else {\n";
        for (auto& st : s.else_branch)
            res += stmtToString(*st, indentLevel + 1) + "\n";
        res += indent(indentLevel) + " }";
        return res;
    }
    std::string operator()(const While& s) const {
        std::string res = indent(indentLevel) + "while (" + expToString(*s.cond) + ") {\n";
        for (auto& st : s.body)
            res += stmtToString(*st, indentLevel + 1) + "\n";
        res += indent(indentLevel) + " }";
        return res;
    }
    std::string operator()(const Break&) const { return indent(indentLevel) + "break;"; }
    std::string operator()(const Continue&) const { return indent(indentLevel) + "continue;"; }
};

std::string stmtToString(const Stmt& stmt, int indentLevel) {
    return std::visit(StmtToStringVisitor{indentLevel}, stmt.val);
}

std::string protoToString(const Proto& p) {
    std::ostringstream oss;
    for (size_t i = 0; i < p.annotations.size(); ++i) {
        oss << "@" << p.annotations[i] << "\n";
    }
    oss << "proto " << p.fname << "(";
    for (size_t i = 0; i < p.args.size(); ++i) {
        const auto& t = p.args[i];
        oss << tyToString(t);
        if (i + 1 < p.args.size())
            oss << ", ";
    }
    oss << ") -> ";
    switch (p.frtyp.tag) {
        case RetTyTag::RetVoid:
            oss << "void";
            break;
        case RetTyTag::RetVal:
            oss << tyToString(*p.frtyp.val);
            break;
    }
    oss << ";";
    return oss.str();
}

std::string fdeclToString(const FDecl& f) {
    std::ostringstream oss;
    oss << "fn " << f.fname << "(";
    for (size_t i = 0; i < f.args.size(); ++i) {
        const auto& p = f.args[i];
        oss << tyToString(p.first) << " " << p.second;
        if (i + 1 < f.args.size())
            oss << ", ";
    }
    oss << ") -> ";
    switch (f.frtyp.tag) {
        case RetTyTag::RetVoid:
            oss << "void";
            break;
        case RetTyTag::RetVal:
            oss << tyToString(*f.frtyp.val);
            break;
    }
    oss << " {\n";
    for (auto& st : f.body) {
        oss << stmtToString(*st, 1) << "\n";
    }
    oss << "}";
    return oss.str();
}

std::string cdeclToString(const CDecl& c) {
    std::ostringstream oss;
    oss << "class " << c.cname << " {\n";

    for (const auto& fld : c.fields) {
        for (auto& pre : fld.prelude) {
            oss << indent(1) << stmtToString(*pre, 1) << "\n";
        }
        oss << indent(1) << fld.fieldName << ": " << tyToString(fld.ftyp) << " = " << expToString(*fld.init) << ";\n";
    }

    oss << "};";
    return oss.str();
}

std::string programToString(const Program& prog) {
    std::ostringstream oss;
    for (const auto& c : prog.classes) {
        oss << cdeclToString(c) << "\n\n";
    }
    for (const auto& p : prog.protos) {
        oss << protoToString(p) << "\n\n";
    }
    for (const auto& f : prog.functions) {
        oss << fdeclToString(f) << "\n\n";
    }
    return oss.str();
}
