#include "value.h"
#include "gc.h"
#include "pair.h"
#include "util.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Value* value_get_nil(void)
{
    static Value nil_val = { .type = VALUE_NIL };
    return &nil_val;
}

Value* value_int_create(long n)
{
    Value* v = gc_alloc(VALUE_INT);
    v->u.integer = n;
    return v;
}

Value* value_symbol_create(const char* s)
{
    Value* v = gc_alloc(VALUE_SYMBOL);
    v->u.symbol = xstrdup(s);
    return v;
}

Value* value_string_create(const char* s)
{
    Value* v = gc_alloc(VALUE_STRING);
    v->u.string = xstrdup(s);
    return v;
}

Value* value_cons_create(Value* a, Value* d)
{
    Value* v = gc_alloc(VALUE_PAIR);
    v->u.pair.car = a;
    v->u.pair.cdr = d;
    return v;
}

Value* value_prim_create(const char* name, PrimFn f)
{
    Value* v = gc_alloc(VALUE_PRIMITIVE);
    v->u.prim.name = name;
    v->u.prim.fn = f;
    return v;
}

Value* value_closure_create(Value* params, Value* body, Value* env)
{
    Value* v = gc_alloc(VALUE_CLOSURE);
    v->u.closure.params = params;
    v->u.closure.body = body;
    v->u.closure.env = env;
    return v;
}

Value* value_macro_create(Value* params, Value* body, Value* env)
{
    Value* v = gc_alloc(VALUE_MACRO);
    v->u.macro.params = params;
    v->u.macro.body = body;
    v->u.macro.env = env;
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
    case VALUE_STRING:
        printf("%s", v->u.string);
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
    case VALUE_MACRO:
        printf("<macro>");
        break;
    default:
        printf("<unknown>");
        break;
    }
}
