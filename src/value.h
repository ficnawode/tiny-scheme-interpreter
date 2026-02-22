#pragma once
#include <stdbool.h>
#include "numbers.h"
#include <stddef.h>

typedef enum {
    VALUE_NIL,
    VALUE_NUM,
    VALUE_SYMBOL,
    VALUE_STRING,
    VALUE_PAIR,
    VALUE_VECTOR,
    VALUE_PRIMITIVE,
    VALUE_CLOSURE,
    VALUE_MACRO,
    VALUE_SYNTAX_RULES,
    VALUE_ERROR
} ValueType;

typedef struct Value Value;
typedef Value* (*PrimFn)(Value* args);

typedef struct SyntaxRule SyntaxRule;
typedef struct SyntaxRules SyntaxRules;

typedef struct {
    size_t length;
    struct Value** data;
} Vector;

struct Value {
    ValueType type;

    union {
        SchemeNum num;
        char* symbol;
        char* string;

        struct
        {
            Value *car, *cdr;
        } pair;

        Vector vector;

        struct
        {
            const char* name;
            PrimFn fn;
        } prim;

        struct
        {
            Value *params, *body, *env;
        } closure;

        struct
        {
            Value *params, *body, *env;
        } macro;

        SyntaxRules* syntax_rules;

        struct
        {
            char* message;
            Value* call_stack;
        } error;
    } u;
};

Value* value_get_nil(void);

Value* value_num_create(SchemeNum n);
Value* value_symbol_create(const char* s);
Value* value_string_create(const char* s);
Value* value_cons_create(Value* a, Value* d);
Value* value_vector_create(size_t length, Value* fill);
Value* value_prim_create(const char* name, PrimFn f);
Value* value_closure_create(Value* params, Value* body, Value* env);
Value* value_macro_create(Value* params, Value* body, Value* env);
Value* value_error_create(const char* message);
Value* value_syntax_rules_create(SyntaxRules* sr);

bool value_is_true(const Value* v);
bool value_equal(const Value* a, const Value* b);

void value_print(const Value* v);
