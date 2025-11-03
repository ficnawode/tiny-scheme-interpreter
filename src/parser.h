#pragma once
#include "lexer.h"
#include "value.h"

typedef struct Parser {
    Lexer* lexer;
    Token current;
} Parser;

Parser* parser_create(const char* source);
void parser_cleanup(Parser* p);

void parser_advance(Parser* p);
Value* parse_expr(Parser* p);
Value* parse_from_string(const char* s);
