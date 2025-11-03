#pragma once

typedef enum {
    VALUE_NIL,
    VALUE_INT,
    VALUE_SYMBOL,
    VALUE_PAIR,
    VALUE_PRIMITIVE,
    VALUE_CLOSURE
} ValueType;

typedef struct Value Value;
typedef Value* (*PrimFn)(Value* args);

struct Value {
    ValueType type;

    union {
        long integer;
        char* symbol;

        struct
        {
            Value *car, *cdr;
        } pair;

        struct
        {
            const char* name;
            PrimFn fn;
        } prim;

        struct
        {
            Value *params, *body, *env;
        } closure;
    } u;
};

Value* value_get_nil(void);

Value* value_int_create(long n);
Value* value_symbol_create(const char* s);
Value* value_cons_create(Value* a, Value* d);
Value* value_prim_create(const char* name, PrimFn f);
Value* value_closure_create(Value* params, Value* body, Value* env);

int value_is_true(Value* v);

void value_print(Value* v);
void value_free(Value* v);
