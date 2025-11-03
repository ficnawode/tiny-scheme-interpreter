#include "eval.h"
#include "intern.h"
#include "lexer.h"
#include "pair.h"
#include "parser.h"
#include "prims.h"
#include "util.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Value* lookup_in_frame(Value* frame, Value* sym)
{
    for (Value* pair = frame; pair != NIL; pair = CDR(pair)) {
        Value* binding = CAR(pair);
        Value* key = CAR(binding);
        if (key->type == VALUE_SYMBOL && strcmp(key->u.symbol, sym->u.symbol) == 0) {
            return CDR(binding);
        }
    }
    return NULL;
}

static Value* env_lookup(Value* env, Value* sym)
{
    for (Value* frame = env; frame != NIL; frame = CDR(frame)) {
        Value* result = lookup_in_frame(CAR(frame), sym);
        if (result != NULL) {
            return result;
        }
    }
    return NULL;
}

static void env_add_binding(Value* env, Value* sym, Value* val)
{
    Value* frame = CAR(env);
    CAR(env) = CONS(CONS(sym, val), frame);
}

static Value* extend_env(Value* env, Value* params, Value* args)
{
    Value* alist = NIL;
    while (params != NIL && args != NIL) {
        Value* sym = CAR(params);
        alist = CONS(CONS(sym, CAR(args)), alist);
        params = CDR(params);
        args = CDR(args);
    }
    return CONS(alist, env);
}

static Value* eval_list(Value* list, Value* env)
{
    if (list == NIL) {
        return list;
    }
    Value* first = eval(CAR(list), env);
    Value* rest = eval_list(CDR(list), env);
    return CONS(first, rest);
}

static Value* eval_int(Value* expr)
{
    return expr;
}

static Value* eval_symbol(Value* expr, Value* env)
{
    Value* v = env_lookup(env, expr);
    if (!v) {
        fprintf(stderr, "Unbound variable: %s\n", expr->u.symbol);
        return NIL;
    }
    return v;
}

static Value* eval_quote(Value* expr)
{
    return CADR(expr);
}

static Value* eval_if(Value* expr, Value* env)
{
    Value* cond = CADR(expr);
    Value* then_branch = CADDR(expr);
    Value* else_branch = (CDR(CDR(CDR(expr))) != NIL) ? CADDDR(expr) : NIL;
    return value_is_true(eval(cond, env)) //
        ? eval(then_branch, env) //
        : eval(else_branch, env);
}

static Value* eval_define(Value* expr, Value* env)
{
    Value* target = CADR(expr);

    if (target->type == VALUE_SYMBOL) {
        Value* value_expr = CADDR(expr);
        Value* result = eval(value_expr, env);
        env_add_binding(env, target, result);
        return target;
    }

    if (target->type == VALUE_PAIR && CAR(target)->type == VALUE_SYMBOL) {
        Value* name = CAR(target);
        Value* params = CDR(target);
        Value* body = CDR(CDR(expr));
        Value* lambda = CONS(intern("lambda"), CONS(params, body));
        env_add_binding(env, name, eval(lambda, env));
        return name;
    }

    fprintf(stderr, "define: invalid form\n");
    return NIL;
}

static Value* eval_lambda(Value* expr, Value* env)
{
    Value* params = CADR(expr);
    Value* body = CDR(CDR(expr));
    return value_closure_create(params, body, env);
}

static Value* eval_function_application(Value* op, Value* expr, Value* env)
{
    Value* proc = eval(op, env);
    Value* args = eval_list(CDR(expr), env);
    return apply(proc, args);
}

static Value* eval_pair(Value* expr, Value* env)
{
    Value* op = CAR(expr);

    if (op->type == VALUE_SYMBOL) {
        const char* s = op->u.symbol;

        if (strcmp(s, "quote") == 0)
            return eval_quote(expr);
        if (strcmp(s, "if") == 0)
            return eval_if(expr, env);
        if (strcmp(s, "define") == 0)
            return eval_define(expr, env);
        if (strcmp(s, "lambda") == 0)
            return eval_lambda(expr, env);
    }
    return eval_function_application(op, expr, env);
}

Value* eval(Value* expr, Value* env)
{
    if (expr == NULL)
        return NIL;

    switch (expr->type) {
    case VALUE_INT:
        return eval_int(expr);
    case VALUE_SYMBOL:
        return eval_symbol(expr, env);
    case VALUE_PAIR:
        return eval_pair(expr, env);
    case VALUE_NIL:
    default:
        return NIL;
    }
}

Value* eval_file(const char* filename, Value* env)
{
    char* input = read_file(filename);
    if (!input) {
        return NULL;
    }

    Parser* p = parser_create(input);
    Value* last_result = NIL;

    for (;;) {
        Value* expr = parse_expr(p);
        if (!expr) {
            break;
        }

        last_result = eval(expr, env);
    }

    parser_cleanup(p);
    free(input);
    return last_result;
}

Value* apply_primitive(Value* proc, Value* args)
{
    return proc->u.prim.fn(args);
}

Value* apply_closure(Value* proc, Value* args)
{
    Value* params = proc->u.closure.params;
    Value* closure_env = extend_env(proc->u.closure.env, params, args);
    Value* res = NIL;

    for (Value* b = proc->u.closure.body; b != NIL; b = CDR(b)) {
        res = eval(CAR(b), closure_env);
    }
    return res;
}

Value* apply(Value* proc, Value* args)
{
    switch (proc->type) {
    case VALUE_PRIMITIVE:
        return apply_primitive(proc, args);
    case VALUE_CLOSURE:
        return apply_closure(proc, args);
    default:
        fprintf(stderr, "Attempt to call non-function\n");
        return value_get_nil();
    }
}

Value* make_global_env()
{
    Value* frame = NIL;
    Value* env = CONS(frame, NIL);

    PrimTable prims = get_prims();
    for (size_t i = 0; i < prims.count; i++) {
        Value* sym = intern(prims.prims[i].name);
        Value* primv = value_prim_create(prims.prims[i].name, prims.prims[i].fn);
        frame = CONS(CONS(sym, primv), frame);
    }

    frame = CONS(CONS(intern("#t"), intern("#t")), frame);

    CAR(env) = frame;
    return env;
}
