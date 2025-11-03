#include "value.h"
#include "pair.h"
#include "util.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Value* mkval(ValueType t)
{
    Value* v = xmalloc(sizeof(Value));
    v->type = t;
    return v;
}

Value* value_get_nil(void)
{
    static Value nil_val = { .type = VALUE_NIL };
    return &nil_val;
}

Value* value_int_create(long n)
{
    Value* v = mkval(VALUE_INT);
    v->u.integer = n;
    return v;
}

Value* value_symbol_create(const char* s)
{
    Value* v = mkval(VALUE_SYMBOL);
    v->u.symbol = xstrdup(s);
    return v;
}

Value* value_cons_create(Value* a, Value* d)
{
    Value* v = mkval(VALUE_PAIR);
    v->u.pair.car = a;
    v->u.pair.cdr = d;
    return v;
}

Value* value_prim_create(const char* name, PrimFn f)
{
    Value* v = mkval(VALUE_PRIMITIVE);
    v->u.prim.name = name;
    v->u.prim.fn = f;
    return v;
}

Value* value_closure_create(Value* params, Value* body, Value* env)
{
    Value* v = mkval(VALUE_CLOSURE);
    v->u.closure.params = params;
    v->u.closure.body = body;
    v->u.closure.env = env;
    return v;
}

void value_print(Value* v);

void print_list(Value* v)
{
    printf("(");
    int first = 1;
    while (v != NIL && v->type == VALUE_PAIR) {
        if (!first)
            printf(" ");
        value_print(v->u.pair.car);
        first = 0;
        v = v->u.pair.cdr;
    }
    if (v != NIL) {
        printf(" . ");
        value_print(v);
    }
    printf(")");
}

int value_is_true(Value* v)
{
    return !(v == NIL);
}

void value_print(Value* v)
{
    if (!v) {
        printf("<NULL>");
        return;
    }
    switch (v->type) {
    case VALUE_NIL:
        printf("()");
        break;
    case VALUE_INT:
        printf("%ld", v->u.integer);
        break;
    case VALUE_SYMBOL:
        printf("%s", v->u.symbol);
        break;
    case VALUE_PAIR:
        print_list(v);
        break;
    case VALUE_PRIMITIVE:
        printf("<primitive:%s>", v->u.prim.name);
        break;
    case VALUE_CLOSURE:
        printf("<closure>");
        break;
    default:
        printf("<unknown>");
        break;
    }
}
void value_free(Value* v)
{
    if (!v || v->type == VALUE_NIL) {
        return;
    }

    ValueType original_type = v->type;
    v->type = VALUE_NIL;

    switch (original_type) {
    case VALUE_PAIR:
        value_free(v->u.pair.car);
        value_free(v->u.pair.cdr);
        break;
    case VALUE_CLOSURE:
        value_free(v->u.closure.params);
        value_free(v->u.closure.body);
        value_free(v->u.closure.env);
        break;
    case VALUE_SYMBOL:
        v->type = original_type;
        return;
    case VALUE_INT:
    case VALUE_PRIMITIVE:
        break;
    case VALUE_NIL:
        break;
    }

    free(v);
}
