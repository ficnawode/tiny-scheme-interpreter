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
    GC_PUSH(a);
    GC_PUSH(d);
    Value* v = gc_alloc(VALUE_PAIR);
    v->u.pair.car = a;
    v->u.pair.cdr = d;
    GC_POP();
    GC_POP();
    return v;
}

Value* value_vector_create(size_t length, Value* fill)
{
    Value* v = gc_alloc(VALUE_VECTOR);
    v->u.vector.length = length;

    if (length > 0) {
        v->u.vector.data = xmalloc(length * sizeof(Value*));
        for (size_t i = 0; i < length; ++i) {
            v->u.vector.data[i] = fill;
        }
    } else {
        v->u.vector.data = NULL;
    }
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
    GC_PUSH(params);
    GC_PUSH(body);
    GC_PUSH(env);
    Value* v = gc_alloc(VALUE_CLOSURE);
    v->u.closure.params = params;
    v->u.closure.body = body;
    v->u.closure.env = env;
    GC_POP();
    GC_POP();
    GC_POP();
    return v;
}

Value* value_macro_create(Value* params, Value* body, Value* env)
{
    GC_PUSH(params);
    GC_PUSH(body);
    GC_PUSH(env);
    Value* v = gc_alloc(VALUE_MACRO);
    v->u.macro.params = params;
    v->u.macro.body = body;
    v->u.macro.env = env;
    GC_POP();
    GC_POP();
    GC_POP();
    return v;
}

Value* value_syntax_rules_create(SyntaxRules* sr)
{
    Value* v = gc_alloc(VALUE_SYNTAX_RULES);
    v->u.syntax_rules = sr;
    return v;
}

Value* value_error_create(const char* message)
{
    Value* v = gc_alloc(VALUE_ERROR);
    v->u.error.message = xstrdup(message);
    v->u.error.call_stack = NIL;
    return v;
}

bool value_is_true(const Value* v)
{
    if (v && v->type == VALUE_SYMBOL && strcmp(v->u.symbol, "#f") == 0) {
        return false;
    }
    return true;
}

bool value_equal(const Value* a, const Value* b)
{
    if (!a || !b) {
        return false;
    }

    if (a == b) {
        return true;
    }

    if (a->type != b->type) {
        return false;
    }

    switch (a->type) {
    case VALUE_INT:
        return a->u.integer == b->u.integer;

    case VALUE_STRING:
        return strcmp(a->u.string, b->u.string) == 0;

    case VALUE_SYMBOL:
        if (a->u.symbol == b->u.symbol) {
            return true;
        }
        return strcmp(a->u.symbol, b->u.symbol) == 0;

    case VALUE_PAIR:
        return value_equal(a->u.pair.car, b->u.pair.car) && value_equal(a->u.pair.cdr, b->u.pair.cdr);
    case VALUE_VECTOR:
        if (a->u.vector.length != b->u.vector.length)
            return false;
        for (size_t i = 0; i < a->u.vector.length; i++) {
            if (!value_equal(a->u.vector.data[i], b->u.vector.data[i]))
                return false;
        }
        return true;
    default:
        return false;
    }
}

static void print_list(const Value* v)
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

void value_print(const Value* v)
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
    case VALUE_VECTOR:
        printf("#(");
        for (size_t i = 0; i < v->u.vector.length; ++i) {
            if (i > 0)
                printf(" ");
            value_print(v->u.vector.data[i]);
        }
        printf(")");
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
    case VALUE_ERROR:
        printf("<error>: %s", v->u.error.message);
        break;
    default:
        printf("<unknown>");
        break;
    }
}
