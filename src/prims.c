#include "prims.h"
#include "error.h"
#include "eval.h"
#include "gc.h"
#include "intern.h"
#include "pair.h"
#include "util.h"
#include "value.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Value* prim_plus(Value* args)
{
    long sum = 0;
    for (Value* p = args; p != NIL; p = CDR(p)) {
        Value* a = CAR(p);
        if (a->type != VALUE_INT) {
            return runtime_error("+: all arguments must be numbers");
        }
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
    if (first->type != VALUE_INT) {
        return runtime_error("-: all arguments must be numbers");
    }

    long result = first->u.integer;
    Value* rest = CDR(args);

    if (rest == NIL) {
        return value_int_create(-result);
    }

    for (Value* p = rest; p != NIL; p = CDR(p)) {
        Value* a = CAR(p);
        if (a->type != VALUE_INT) {
            return runtime_error("-: all arguments must be numbers");
        }
        result -= a->u.integer;
    }
    return value_int_create(result);
}

Value* prim_mul(Value* args)
{
    long prod = 1;
    for (Value* p = args; p != NIL; p = CDR(p)) {
        Value* a = CAR(p);
        if (a->type != VALUE_INT) {
            return runtime_error("*: all arguments must be numbers");
        }
        prod *= a->u.integer;
    }
    return value_int_create(prod);
}

Value* prim_div(Value* args)
{
    int n_args = list_length(args);
    if (n_args < 2) {
        return runtime_error("/: expects at least 2 arguments");
    }

    Value* first = CAR(args);
    if (first->type != VALUE_INT) {
        return runtime_error("/: all arguments must be numbers");
    }
    long result = first->u.integer;

    for (Value* p = CDR(args); p != NIL; p = CDR(p)) {
        Value* a = CAR(p);
        if (a->type != VALUE_INT) {
            return runtime_error("/: all arguments must be numbers");
        }
        if (a->u.integer == 0) {
            return runtime_error("division by zero");
        }
        result /= a->u.integer;
    }
    return value_int_create(result);
}

Value* prim_eq(Value* args)
{
    if (list_length(args) != 2) {
        return runtime_error("=: expects exactly 2 arguments");
    }
    Value* a = CAR(args);
    Value* b = CADR(args);
    if (a->type != VALUE_INT || b->type != VALUE_INT) {
        return runtime_error("=: arguments must be numbers");
    }
    return (a->u.integer == b->u.integer) ? intern("#t") : intern("#f");
}

Value* prim_lt(Value* args)
{
    if (list_length(args) != 2) {
        return runtime_error("<: expects exactly 2 arguments");
    }
    Value* a = CAR(args);
    Value* b = CADR(args);
    if (a->type != VALUE_INT || b->type != VALUE_INT) {
        return runtime_error("<: arguments must be numbers");
    }
    return (a->u.integer < b->u.integer) ? intern("#t") : intern("#f");
}

Value* prim_gt(Value* args)
{
    if (list_length(args) != 2) {
        return runtime_error(">: expects exactly 2 arguments");
    }
    Value* a = CAR(args);
    Value* b = CADR(args);
    if (a->type != VALUE_INT || b->type != VALUE_INT) {
        return runtime_error(">: arguments must be numbers");
    }
    return (a->u.integer > b->u.integer) ? intern("#t") : intern("#f");
}

Value* prim_number_p(Value* args)
{
    if (list_length(args) != 1) {
        return runtime_error("number?: expects exactly 1 argument");
    }
    return (CAR(args)->type == VALUE_INT) ? intern("#t") : intern("#f");
}

Value* prim_cons(Value* args)
{
    if (list_length(args) != 2) {
        return runtime_error("cons: expects exactly 2 arguments");
    }
    return value_cons_create(CAR(args), CADR(args));
}

Value* prim_car(Value* args)
{
    if (list_length(args) != 1) {
        return runtime_error("car: expects exactly 1 argument");
    }
    Value* pair = CAR(args);
    if (pair->type != VALUE_PAIR) {
        return runtime_error("car: argument must be a pair");
    }
    return CAR(pair);
}

Value* prim_cdr(Value* args)
{
    if (list_length(args) != 1) {
        return runtime_error("cdr: expects exactly 1 argument");
    }
    Value* pair = CAR(args);
    if (pair->type != VALUE_PAIR) {
        return runtime_error("cdr: argument must be a pair");
    }
    return CDR(pair);
}

Value* prim_list_p(Value* args)
{
    if (list_length(args) != 1) {
        return runtime_error("list?: expects exactly 1 argument");
    }

    Value* p = CAR(args);
    for (;;) {
        if (p == NIL)
            return intern("#t");
        if (p->type != VALUE_PAIR)
            return intern("#f");
        p = CDR(p);
    }
}

Value* prim_eq_p(Value* args)
{
    if (list_length(args) != 2) {
        return runtime_error("eq?: expects exactly 2 arguments");
    }
    Value* a = CAR(args);
    Value* b = CADR(args);
    return (a == b) ? intern("#t") : intern("#f");
}

Value* prim_atom_p(Value* args)
{
    if (list_length(args) != 1) {
        return runtime_error("atom?: expects exactly 1 argument");
    }
    Value* a = CAR(args);
    return (a == NIL || a->type != VALUE_PAIR) ? intern("#t") : intern("#f");
}

Value* prim_string_p(Value* args)
{
    if (list_length(args) != 1) {
        return runtime_error("string?: expects exactly 1 argument");
    }
    return (CAR(args)->type == VALUE_STRING) ? intern("#t") : intern("#f");
}

Value* prim_string_length(Value* args)
{
    if (list_length(args) != 1) {
        return runtime_error("string-length: expects exactly 1 argument");
    }
    Value* str = CAR(args);
    if (str->type != VALUE_STRING) {
        return runtime_error("string-length: argument must be a string");
    }
    return value_int_create(strlen(str->u.string));
}

Value* prim_string_append(Value* args)
{
    size_t total_len = 0;
    for (Value* p = args; p != NIL; p = CDR(p)) {
        Value* str = CAR(p);
        if (str->type != VALUE_STRING) {
            return runtime_error("string-append: all arguments must be strings");
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
        return runtime_error("gensym: expects 0 or 1 arguments");
    }

    if (n_args == 1) {
        Value* prefix_val = CAR(args);
        if (prefix_val->type != VALUE_STRING) {
            return runtime_error("gensym: argument must be a string");
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
    if (list_length(args) != 1) {
        return runtime_error("null?: expects exactly 1 argument");
    }
    return (CAR(args) == NIL) ? intern("#t") : intern("#f");
}

Value* prim_error(Value* args)
{
    if (args == NIL) {
        return runtime_error("error: procedure called with no arguments");
    }
    Value* msg = CAR(args);
    if (msg->type != VALUE_STRING) {
        return runtime_error("error: first argument must be a string message");
    }
    return runtime_error(msg->u.string);
}

Value* prim_error_object_p(Value* args)
{
    if (list_length(args) != 1) {
        return runtime_error("error-object?: expects exactly 1 argument");
    }
    return (CAR(args)->type == VALUE_ERROR) ? intern("#t") : intern("#f");
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
        { "newline", prim_newline },
        { "error", prim_error },
        { "error-object?", prim_error_object_p }
    };
    size_t prims_len = sizeof(prims) / sizeof(prims[0]);
    return (PrimTable) { .prims = prims, .count = prims_len };
}
