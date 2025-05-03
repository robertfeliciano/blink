#pragma once

#include <vector>

#include <ast/decl.h>

using Program = std::vector<Decl>;

Program convert_program(value v);

std::string programToString(const Program& prog);