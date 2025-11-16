#pragma once
#include "lexer.h"

void print_parser_error(Location loc, const char* source_code, const char* format, ...);
