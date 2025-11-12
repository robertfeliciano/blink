#include <stdexcept>
#include <iostream>
#include <sstream>

#include <caml/mlvalues.h>
#include <caml/custom.h>

#include <bridge/stmt.h>
#include <bridge/exp.h>
#include <bridge/types.h>

static std::vector<std::unique_ptr<Stmt>> convert_block(value v) {
    std::vector<std::unique_ptr<Stmt>> out;
    while (v != Val_emptylist) {
        value head = Field(v, 0);
        out.push_back(std::make_unique<Stmt>(convert_stmt(head)));
        v = Field(v, 1);
    }
    return out;
}

Stmt convert_stmt(value v) {
    Stmt result;

    if (!Is_block(v))
        throw std::runtime_error("Expected block for stmt variant");

    switch (Tag_val(v)) {
        case 0: { // Assn of exp * exp * ty
            auto lhs = std::make_unique<Exp>(convert_exp(Field(v, 0)));
            auto rhs = std::make_unique<Exp>(convert_exp(Field(v, 1)));
            Ty ty = convert_ty(Field(v, 2));
            result.val = Assn{ std::move(lhs), std::move(rhs), std::move(ty) };
            break;
        }
        case 1: { // LambdaDecl of ldecl
            throw std::runtime_error("Lambda declarations not supported in bridge conversion");
        }
        case 2: { // Decl of vdecl (id * ty * exp * bool)
            std::string id = String_val(Field(v, 0));
            Ty ty = convert_ty(Field(v, 1));
            auto init = std::make_unique<Exp>(convert_exp(Field(v, 2)));
            bool is_const = Bool_val(Field(v, 3));
            result.val = VDecl{ id, std::move(ty), std::move(init), is_const };
            break;
        }
        case 3: { // Ret of exp option
            value opt = Field(v, 0);
            Ret r;
            if (Is_block(opt)) {
                r.value = std::make_optional<std::unique_ptr<Exp>>(std::make_unique<Exp>(convert_exp(Field(opt, 0))));
            } else {
                r.value = std::nullopt;
            }
            result.val = std::move(r);
            break;
        }
        case 4: { // SCall of exp * exp list
            auto callee = std::make_unique<Exp>(convert_exp(Field(v, 0)));
            value args_v = Field(v, 1);
            std::vector<std::unique_ptr<Exp>> args;
            while (args_v != Val_emptylist) {
                value head = Field(args_v, 0);
                args.push_back(std::make_unique<Exp>(convert_exp(head)));
                args_v = Field(args_v, 1);
            }
            result.val = SCall{ std::move(callee), std::move(args) };
            break;
        }
        case 5: { // If of exp * block * block
            auto cond = std::make_unique<Exp>(convert_exp(Field(v, 0)));
            auto then_b = convert_block(Field(v, 1));
            auto else_b = convert_block(Field(v, 2));
            result.val = If{ std::move(cond), std::move(then_b), std::move(else_b) };
            break;
        }
        case 6: { // For of id * exp * exp * bool * exp * ty * block
            std::string id = String_val(Field(v, 0));
            auto start = std::make_unique<Exp>(convert_exp(Field(v, 1)));
            auto end = std::make_unique<Exp>(convert_exp(Field(v, 2)));
            bool incl = Bool_val(Field(v, 3));
            auto step = std::make_unique<Exp>(convert_exp(Field(v, 4)));
            Ty iter_ty = convert_ty(Field(v, 5));
            auto body = convert_block(Field(v, 6));
            result.val = For{ id, std::move(start), std::move(end), incl, std::move(step), std::move(iter_ty), std::move(body) };
            break;
        }
        case 7: { // ForEach of id * exp * ty * block
            std::string iterator = String_val(Field(v, 0));
            auto iterable = std::make_unique<Exp>(convert_exp(Field(v, 1)));
            Ty iter_ty = convert_ty(Field(v, 2));
            auto body = convert_block(Field(v, 4));
            result.val = ForEach{ iterator, std::move(iterable), std::move(iter_ty), std::move(body) };
        }
        case 8: { // While of exp * block
            auto cond = std::make_unique<Exp>(convert_exp(Field(v, 0)));
            auto body = convert_block(Field(v, 1));
            result.val = While{ std::move(cond), std::move(body) };
            break;
        }
        case 9: { // Break
            result.val = Break{};
            break;
        }
        case 10: { // Continue
            result.val = Continue{};
            break;
        }
        default: {
            throw std::runtime_error("Unsupported stmt variant in bridge conversion");
        }
    }

    return result;
}

static std::string indent(int level) {
    return std::string(level * 4, ' ');
}

struct StmtToStringVisitor {
    int indentLevel;

    std::string operator()(const Assn& s) const {
        return indent(indentLevel) + expToString(*s.lhs) + " = " + expToString(*s.rhs) + ";";
    }
    std::string operator()(const VDecl& s) const {
        return indent(indentLevel) + (s.is_const ? "const " : "var ") + s.id + ": " + tyToString(s.ty) + " = " + expToString(*s.init) + ";";
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
            if (i + 1 < s.args.size()) res += ", ";
        }
        res += ");";
        return res;
    }
    std::string operator()(const If& s) const {
        std::string res = indent(indentLevel) + "if (" + expToString(*s.cond) + ") {\n";
        for (auto &st : s.then_branch) res += stmtToString(*st, indentLevel + 1) + "\n";
        res += indent(indentLevel) + " } else {\n";
        for (auto &st : s.else_branch) res += stmtToString(*st, indentLevel + 1) + "\n";
        res += indent(indentLevel) + " }";
        return res;
    }
    std::string operator()(const For& s) const {
        std::string res = indent(indentLevel) + "for (" + s.id + " = " + expToString(*s.start) + "; " + s.id + " < " + expToString(*s.end);
        res += (s.incl ? " <= " : " < ");
        res += expToString(*s.end) + ") {\n";
        for (auto &st : s.body) res += stmtToString(*st, indentLevel + 1) + "\n";
        res += indent(indentLevel) + " }";
        return res;
    }
    std::string operator()(const ForEach& s) const {
        std::string res = indent(indentLevel) + "for (" + s.iterator + " in " + expToString(*s.iterable) + " {\n";
        for (auto &st : s.body) res += stmtToString(*st, indentLevel + 1) + "\n";
        res += indent(indentLevel) + "}";
        return res;
    }
    std::string operator()(const While& s) const {
        std::string res = indent(indentLevel) + "while (" + expToString(*s.cond) + ") {\n";
        for (auto &st : s.body) res += stmtToString(*st, indentLevel + 1) + "\n";
        res += indent(indentLevel) + " }";
        return res;
    }
    std::string operator()(const Break&) const {
        return indent(indentLevel) + "break;";
    }
    std::string operator()(const Continue&) const {
        return indent(indentLevel) + "continue;";
    }
};

std::string stmtToString(const Stmt& stmt, int indentLevel) {
    return std::visit(StmtToStringVisitor{indentLevel}, stmt.val);
}
