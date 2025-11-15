#include "prims.h"
#include "eval.h"
#include "intern.h"
#include "pair.h"
#include "util.h"
#include "value.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int list_length(Value* args)
{
    int n = 0;
    while (args != NIL && args->type == VALUE_PAIR) {
        n++;
        args = CDR(args);
    }
    return n;
}

static bool expect_type(ValueType type, Value* v, const char* name)
{
    if (v->type != type) {
        fprintf(stderr, "%s: expected type %d but got %d \n", name, type, v->type);
        return false;
    }
    return true;
}

static bool expect_n_args(Value* args, int n_args_expected, const char* name)
{
    if (list_length(args) != n_args_expected) {
        fprintf(stderr, "%s: expects %d arguments\n", name, n_args_expected);
        return false;
    }
    return true;
}

Value* prim_plus(Value* args)
{
    long sum = 0;
    for (; args != NIL; args = CDR(args)) {
        Value* a = CAR(args);
        if (!expect_type(VALUE_INT, a, "+"))
            return NIL;
        sum += a->u.integer;
    }
    return value_int_create(sum);
}

Value* prim_minus(Value* args)
{
    if (args == NIL) {
        return value_int_create(0);
    }

    Value* first = CAR(args);
    if (!expect_type(VALUE_INT, first, "-")) {
        return NIL;
    }

    long result = first->u.integer;
    args = CDR(args);

    if (args == NIL) {
        return value_int_create(-result);
    }

    for (; args != NIL; args = CDR(args)) {
        Value* a = CAR(args);
        if (!expect_type(VALUE_INT, a, "-")) {
            return NIL;
        }
        result -= a->u.integer;
    }
    return value_int_create(result);
}

Value* prim_mul(Value* args)
{
    long prod = 1;
    for (; args != NIL; args = CDR(args)) {
        Value* a = CAR(args);
        if (!expect_type(VALUE_INT, a, "*")) {
            return NIL;
        }
        prod *= a->u.integer;
    }
    return value_int_create(prod);
}

Value* prim_div(Value* args)
{
    if (args == NIL || CDR(args) == NIL) {
        fprintf(stderr, "/: expects at least 2 arguments\n");
        return NIL;
    }
    Value* first = CAR(args);
    if (!expect_type(VALUE_INT, first, "/")) {
        return NIL;
    }
    long result = first->u.integer;

    for (args = CDR(args); args != NIL; args = CDR(args)) {
        Value* a = CAR(args);
        if (!expect_type(VALUE_INT, a, "/")) {
            return NIL;
        }
        if (a->u.integer == 0) {
            fprintf(stderr, "/: division by zero\n");
            return NIL;
        }
        result /= a->u.integer;
    }
    return value_int_create(result);
}

Value* prim_eq(Value* args)
{
    if (!expect_n_args(args, 2, "=")) {
        return NIL;
    }
    Value* a = CAR(args);
    Value* b = CADR(args);
    if (a->type != VALUE_INT || b->type != VALUE_INT) {
        return NIL;
    }
    return (a->u.integer == b->u.integer) ? intern("#t") : intern("#f");
}
Value* prim_lt(Value* args)
{
    if (!expect_n_args(args, 2, "<")) {
        return NIL;
    }
    Value* a = CAR(args);
    Value* b = CADR(args);
    if (a->type != VALUE_INT || b->type != VALUE_INT) {
        return NIL;
    }
    return (a->u.integer < b->u.integer) ? intern("#t") : intern("#f");
}

Value* prim_gt(Value* args)
{
    if (!expect_n_args(args, 2, ">")) {
        return NIL;
    }
    Value* a = CAR(args);
    Value* b = CADR(args);
    if (a->type != VALUE_INT || b->type != VALUE_INT) {
        return NIL;
    }
    return (a->u.integer > b->u.integer) ? intern("#t") : intern("#f");
}

Value* prim_number_p(Value* args)
{
    if (!expect_n_args(args, 1, "number?")) {
        return NIL;
    }
    Value* a = CAR(args);
    return (a->type == VALUE_INT) ? intern("#t") : intern("#f");
}

Value* prim_cons(Value* args)
{
    if (!expect_n_args(args, 2, "cons")) {
        return NIL;
    }
    return value_cons_create(CAR(args), CADR(args));
}

Value* prim_car(Value* args)
{
    if (!expect_n_args(args, 1, "car")
        || !expect_type(VALUE_PAIR, CAR(args), "car")) {
        return NIL;
    }
    return CAR(CAR(args));
}

Value* prim_cdr(Value* args)
{
    if (!expect_n_args(args, 1, "cdr")
        || !expect_type(VALUE_PAIR, CAR(args), "cdr")) {
        return NIL;
    }
    return CDR(CAR(args));
}

Value* prim_list_p(Value* args)
{
    if (!expect_n_args(args, 1, "list?")) {
        return NIL;
    }

    Value* p = CAR(args);

    for (;;) {
        if (p == NIL) {
            return intern("#t");
        }

        if (p->type != VALUE_PAIR) {
            return intern("#f");
        }

        p = CDR(p);
    }
}

Value* prim_eq_p(Value* args)
{
    if (!expect_n_args(args, 2, "eq?")) {
        return NIL;
    }
    Value* a = CAR(args);
    Value* b = CADR(args);
    return (a == b) ? intern("#t") : intern("#f");
}

Value* prim_atom_p(Value* args)
{
    if (!expect_n_args(args, 1, "atom?")) {
        return NIL;
    }
    Value* a = CAR(args);
    return (a == NIL || a->type != VALUE_PAIR) ? intern("#t") : intern("#f");
}

Value* prim_string_p(Value* args)
{
    if (!expect_n_args(args, 1, "string?")) {
        return NIL;
    }
    return (CAR(args)->type == VALUE_STRING) ? intern("#t") : intern("#f");
}

Value* prim_string_length(Value* args)
{
    Value* str = CAR(args);
    if (!expect_n_args(args, 1, "string-length")
        || !expect_type(VALUE_STRING, str, "string-length")) {
        return NIL;
    }
    return value_int_create(strlen(str->u.string));
}

Value* prim_string_append(Value* args)
{
    size_t total_len = 0;
    for (Value* p = args; p != NIL; p = CDR(p)) {
        Value* str = CAR(p);
        if (!expect_type(VALUE_STRING, str, "string-append")) {
            return NIL;
        }
        total_len += strlen(str->u.string);
    }

    char* result_str = xmalloc(total_len + 1);
    result_str[0] = '\0';

    for (Value* p = args; p != NIL; p = CDR(p)) {
        strcat(result_str, CAR(p)->u.string);
    }

    Value* result = value_string_create(result_str);
    free(result_str);
    return result;
}

Value* prim_gensym(Value* args)
{
    static unsigned long gensym_counter = 0;
    const char* prefix = "__GENSYM";
    int n_args = list_length(args);

    if (n_args > 1) {
        fprintf(stderr, "gensym: expects 0 or 1 arguments\n");
        return NIL;
    }

    if (n_args == 1) {
        Value* prefix_val = CAR(args);
        if (!expect_type(VALUE_STRING, prefix_val, "gensym")) {
            return NIL;
        }
        prefix = prefix_val->u.string;
    }

    char buffer[256];
    snprintf(buffer, sizeof(buffer), "%s__%lu", prefix, gensym_counter++);

    return value_symbol_create(buffer);
}

Value* prim_display(Value* args)
{
    for (Value* p = args; p != value_get_nil(); p = p->u.pair.cdr) {
        value_print(p->u.pair.car);
    }
    return value_get_nil();
}

Value* prim_newline(Value* args)
{
    (void)args;
    printf("\n");
    return value_get_nil();
}

Value* prim_null_p(Value* args)
{
    if (!expect_n_args(args, 1, "null?")) {
        return NIL;
    }
    Value* a = CAR(args);
    return (a == NIL) ? intern("#t") : intern("#f");
}

PrimTable get_prims(void)
{
    static PrimDef prims[] = {
        { "+", prim_plus },
        { "-", prim_minus },
        { "*", prim_mul },
        { "/", prim_div },
        { "=", prim_eq },
        { ">", prim_gt },
        { "<", prim_lt },
        { "number?", prim_number_p },

        { "cons", prim_cons },
        { "car", prim_car },
        { "cdr", prim_cdr },
        { "list?", prim_list_p },

        { "eq?", prim_eq_p },
        { "atom?", prim_atom_p },
        { "null?", prim_null_p },

        { "string?", prim_string_p },
        { "string-length", prim_string_length },
        { "string-append", prim_string_append },

        { "gensym", prim_gensym },

        { "display", prim_display },
        { "newline", prim_newline }
    };
    size_t prims_len = sizeof(prims) / sizeof(prims[0]);
    return (PrimTable) { .prims = prims, .count = prims_len };
}
