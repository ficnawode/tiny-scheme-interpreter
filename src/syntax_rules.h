#pragma once

#include "value.h"

typedef struct SyntaxRule {
    Value* pattern;
    Value* template;
} SyntaxRule;

typedef struct SyntaxRules {
    Value* literals;
    Value* defining_env;
    unsigned int rule_count;
    SyntaxRule* rules;
} SyntaxRules;

Value* parse_define_syntax(Value* expr, Value* env);
Value* expand_syntax_rules(Value* macro_val, Value* expr);
void syntax_rules_free(SyntaxRules* sr);
