#include "prims.h"
#include "eval.h"
#include "pair.h"
#include "value.h"

#include <stdio.h>
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

static Value* expect_type(ValueType type, Value* v, const char* name)
{
    if (v->type != type) {
        fprintf(stderr, "%s: expected type %d\n", name, type);
        return NULL;
    }
    return v;
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
    if (list_length(args) != 2) {
        fprintf(stderr, "=: expects 2 arguments\n");
        return NIL;
    }
    Value* a = CAR(args);
    Value* b = CADR(args);
    if (a->type != VALUE_INT || b->type != VALUE_INT) {
        return NIL;
    }
    return (a->u.integer == b->u.integer) ? intern("#t") : NIL;
}

Value* prim_cons(Value* args)
{
    if (list_length(args) != 2) {
        fprintf(stderr, "cons expects 2 args\n");
        return NIL;
    }
    return value_cons_create(CAR(args), CADR(args));
}

Value* prim_car(Value* args)
{
    if (list_length(args) != 1) {
        fprintf(stderr, "car expects 1 arg\n");
        return NIL;
    }
    if (CAR(args)->type != VALUE_PAIR) {
        fprintf(stderr, "car: expected pair\n");
        return NIL;
    }
    return CAR(CAR(args));
}

Value* prim_cdr(Value* args)
{
    if (list_length(args) != 1) {
        fprintf(stderr, "cdr expects 1 arg\n");
        return NIL;
    }
    if (CAR(args)->type != VALUE_PAIR) {
        fprintf(stderr, "cdr: expected pair\n");
        return NIL;
    }
    return CDR(CAR(args));
}

Value* prim_eqp(Value* args)
{
    if (list_length(args) != 2) {
        fprintf(stderr, "eq? expects 2 args\n");
        return NIL;
    }
    Value* a = CAR(args);
    Value* b = CADR(args);
    return (a == b) ? intern("#t") : NIL;
}

Value* prim_atom(Value* args)
{
    if (list_length(args) != 1) {
        fprintf(stderr, "atom? expects 1 arg\n");
        return NIL;
    }
    Value* a = CAR(args);
    return (a == NIL || a->type != VALUE_PAIR) ? intern("#t") : NIL;
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

PrimTable get_prims(void)
{
    static PrimDef prims[] = { { "+", prim_plus },
        { "-", prim_minus },
        { "*", prim_mul },
        { "/", prim_div },
        { "=", prim_eq },
        { "cons", prim_cons },
        { "car", prim_car },
        { "cdr", prim_cdr },
        { "eq?", prim_eqp },
        { "atom?", prim_atom },
        { "display", prim_display },
        { "newline", prim_newline } };

    size_t prims_len = sizeof(prims) / sizeof(prims[0]);
    return (PrimTable) { .prims = prims, .count = prims_len };
}
