#include "eval.h"
#include "gc.h"
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
    Value* binding = CONS(sym, val);

    GC_PUSH(binding);

    Value* frame = CAR(env);

    frame = CONS(binding, frame);

    CAR(env) = frame;

    GC_POP();
}

static Value* extend_env(Value* env, Value* params, Value* args)
{
    Value* alist = NIL;

    GC_PUSH(params);
    GC_PUSH(args);
    GC_PUSH(alist);

    while (params != NIL && args != NIL) {
        Value* sym = CAR(params);
        Value* val = CAR(args);

        Value* binding = CONS(sym, val);

        GC_PUSH(binding);

        alist = CONS(binding, alist);

        GC_POP();

        params = CDR(params);
        args = CDR(args);
    }

    Value* result = CONS(alist, env);

    GC_POP();
    GC_POP();
    GC_POP();

    return result;
}

static Value* eval_list(Value* list, Value* env)
{
    if (list == NIL) {
        return NIL;
    }

    Value* head = NIL;
    Value* tail = NIL;

    GC_PUSH(list);
    GC_PUSH(head);

    for (Value* p = list; p != NIL; p = CDR(p)) {
        Value* evaluated_arg = eval(CAR(p), env);

        GC_PUSH(evaluated_arg);

        Value* node = CONS(evaluated_arg, NIL);

        if (head == NIL) {
            head = tail = node;
        } else {
            CDR(tail) = node;
            tail = node;
        }

        GC_POP();
    }

    GC_POP();
    GC_POP();

    return head;
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
        GC_PUSH(target);
        GC_PUSH(result);
        env_add_binding(env, target, result);
        GC_POP();
        GC_POP();
        return target;
    }

    if (target->type == VALUE_PAIR && CAR(target)->type == VALUE_SYMBOL) {
        Value* name = CAR(target);
        Value* params = CDR(target);
        Value* body = CDR(CDR(expr));

        GC_PUSH(name);
        GC_PUSH(params);
        GC_PUSH(body);

        Value* lambda_guts = CONS(params, body);
        GC_PUSH(lambda_guts);

        Value* lambda = CONS(intern("lambda"), lambda_guts);
        GC_POP();
        GC_PUSH(lambda);

        Value* lambda_val = eval(lambda, env);
        GC_PUSH(lambda_val);

        env_add_binding(env, name, lambda_val);

        GC_POP();
        GC_POP();
        GC_POP();
        GC_POP();
        GC_POP();

        return name;
    }

    fprintf(stderr, "define: invalid form\n");
    return NIL;
}

static Value* eval_lambda(Value* expr, Value* env)
{
    Value* params = CADR(expr);
    Value* body = CDR(CDR(expr));
    GC_PUSH(params);
    GC_PUSH(body);

    Value* clos = value_closure_create(params, body, env);

    GC_POP();
    GC_POP();
    return clos;
}

static Value* eval_function_application(Value* op, Value* expr, Value* env)
{
    Value* proc = eval(op, env);
    GC_PUSH(proc);

    Value* args = eval_list(CDR(expr), env);
    GC_PUSH(args);

    Value* res = apply(proc, args);

    GC_POP();
    GC_POP();

    return res;
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

    GC_PUSH(last_result);

    for (;;) {
        Value* expr = parse_expr(p);
        if (!expr) {
            break;
        }
        last_result = eval(expr, env);
    }

    GC_POP();

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
    GC_PUSH(closure_env);

    Value* result = NIL;
    GC_PUSH(result);

    for (Value* b = proc->u.closure.body; b != NIL; b = CDR(b)) {
        result = eval(CAR(b), closure_env);
    }

    GC_POP();
    GC_POP();

    return result;
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
        return NIL;
    }
}

Value* make_global_env()
{
    Value* env = CONS(NIL, NIL);
    GC_PUSH(env);

    PrimTable prims = get_prims();
    for (size_t i = 0; i < prims.count; i++) {
        Value* sym = intern(prims.prims[i].name);
        GC_PUSH(sym);

        Value* primv = value_prim_create(prims.prims[i].name, prims.prims[i].fn);
        GC_PUSH(primv);

        env_add_binding(env, sym, primv);

        GC_POP();
        GC_POP();
    }

    Value* true_sym = intern("#t");
    GC_PUSH(true_sym);
    env_add_binding(env, true_sym, true_sym);
    GC_POP();

    GC_POP();
    return env;
}
