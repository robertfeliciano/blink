#pragma once

#include <bridge/decl.h>
#include <bridge/exp.h>
#include <bridge/prog.h>
#include <bridge/stmt.h>
#include <bridge/types.h>
#include <string>
#include <vector>

// Type printing
std::string tyToString(const Ty& ty);

// Expression printing
std::string toString(BinOp op);
std::string toString(UnOp op);
std::string expToString(const Exp& exp);

// Statement printing
std::string stmtToString(const Stmt& stmt, int indentLevel = 0);

// Declarations / program printing
std::string fdeclToString(const FDecl& f);
std::string cdeclToString(const CDecl& c);
std::string programToString(const Program& prog);
