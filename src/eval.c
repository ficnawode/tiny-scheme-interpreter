#include "eval.h"
#include "env.h"
#include "gc.h"
#include "intern.h"
#include "lexer.h"
#include "pair.h"
#include "parser.h"
#include "prims.h"
#include "util.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    enum {
        RES_VALUE,
        RES_TAILCALL,
        RES_NO_MATCH
    } kind;
    union {
        struct {
            Value* value;
        } v;
        struct {
            Value* expr;
            Value* env;
        } tc;
    };
} EvalResult;

static inline EvalResult result_value(Value* value)
{
    EvalResult r;
    r.kind = RES_VALUE;
    r.v.value = value;
    return r;
}

static inline EvalResult result_tail(Value* expr, Value* env)
{
    EvalResult r;
    r.kind = RES_TAILCALL;
    r.tc.expr = expr;
    r.tc.env = env;
    return r;
}

static inline EvalResult result_no_match(void)
{
    EvalResult r = { .kind = RES_NO_MATCH };
    return r;
}

static inline bool value_is_symbol_name(Value* v, const char* name)
{
    return (v && v->type == VALUE_SYMBOL && strcmp(v->u.symbol, name) == 0);
}

static Value* eval_args(Value* list, Value* env)
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

static EvalResult handle_quote(Value* expr)
{
    return result_value(CADR(expr));
}

static EvalResult handle_if(Value* expr, Value* env)
{
    Value* cond = CADR(expr);
    Value* then_branch = CADDR(expr);
    Value* else_branch = (CDR(CDR(CDR(expr))) != NIL) ? CADDDR(expr) : NIL;

    Value* cond_result = eval(cond, env);
    Value* next = value_is_true(cond_result) ? then_branch : else_branch;
    return result_tail(next, env);
}

static EvalResult handle_define_symbol(Value* expr, Value* env)
{
    Value* target = CADR(expr);
    Value* value_expr = CADDR(expr);
    Value* result = eval(value_expr, env);
    env_add_binding(env, target, result);
    return result_value(target);
}

static EvalResult handle_define_closure(Value* expr, Value* env)
{
    Value* target = CADR(expr);
    Value* name = CAR(target);
    Value* params = CDR(target);
    Value* body = CDR(CDR(expr));
    Value* closure = value_closure_create(params, body, env);
    env_add_binding(env, name, closure);
    return result_value(name);
}

static EvalResult handle_define(Value* expr, Value* env)
{
    Value* target = CADR(expr);
    if (target->type == VALUE_SYMBOL) {
        return handle_define_symbol(expr, env);
    }

    if (target->type == VALUE_PAIR && CAR(target)->type == VALUE_SYMBOL) {
        return handle_define_closure(expr, env);
    }

    fprintf(stderr, "define: invalid form\n");
    return result_value(NIL);
}

static EvalResult handle_lambda(Value* expr, Value* env)
{
    Value* params = CADR(expr);
    Value* body = CDR(CDR(expr));
    Value* closure = value_closure_create(params, body, env);
    return result_value(closure);
}

static EvalResult try_handle_special_form(Value* expr, Value* env)
{
    Value* op = CAR(expr);

    if (!op || op->type != VALUE_SYMBOL)
        return result_no_match();

    if (value_is_symbol_name(op, "quote"))
        return handle_quote(expr);

    if (value_is_symbol_name(op, "if"))
        return handle_if(expr, env);

    if (value_is_symbol_name(op, "define"))
        return handle_define(expr, env);

    if (value_is_symbol_name(op, "lambda"))
        return handle_lambda(expr, env);

    return result_no_match();
}

static Value* apply_primitive(Value* proc, Value* args)
{
    return proc->u.prim.fn(args);
}

static EvalResult apply_closure(Value* proc, Value* args)
{
    Value* closure_env = env_extend(proc->u.closure.env,
        proc->u.closure.params,
        args);
    GC_PUSH(closure_env);

    Value* body = proc->u.closure.body;

    while (CDR(body) != NIL) {
        eval(CAR(body), closure_env);
        body = CDR(body);
    }

    Value* next_expr = CAR(body);

    GC_POP();

    return result_tail(next_expr, closure_env);
}

static EvalResult apply_proc(Value* proc, Value* args)
{
    switch (proc->type) {
    case VALUE_PRIMITIVE:
        return result_value(apply_primitive(proc, args));
    case VALUE_CLOSURE:
        return apply_closure(proc, args);
    default:
        fprintf(stderr, "Attempt to call non-function\n");
        return result_value(NIL);
    }
}

static EvalResult eval_pair(Value* expr, Value* env)
{
    EvalResult sf = try_handle_special_form(expr, env);
    if (sf.kind != RES_NO_MATCH) {
        return sf;
    }

    Value* op = CAR(expr);
    Value* proc = eval(op, env);
    GC_PUSH(proc);
    Value* args = eval_args(CDR(expr), env);
    GC_PUSH(args);

    EvalResult r = apply_proc(proc, args);

    GC_POP();
    GC_POP();
    return r;
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

static EvalResult eval_dispatch(Value* expr, Value* env)
{
    if (expr == NULL) {
        return result_value(NIL);
    }

    switch (expr->type) {
    case VALUE_INT:
    case VALUE_NIL:
    case VALUE_PRIMITIVE:
    case VALUE_CLOSURE:
    case VALUE_STRING:
        return result_value(expr);
    case VALUE_SYMBOL:
        Value* v = eval_symbol(expr, env);
        return result_value(v);
    case VALUE_PAIR:
        return eval_pair(expr, env);
    default:
        fprintf(stderr, "eval: unknown expression type\n");
        return result_value(NIL);
    }
}

Value* eval(Value* expr, Value* env)
{
    for (;;) {
        EvalResult r = eval_dispatch(expr, env);
        if (r.kind == RES_VALUE) {
            return r.v.value;
        }
        expr = r.tc.expr;
        env = r.tc.env;
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

Value* make_global_env()
{
    Value* env = CONS(NIL, NIL);
    GC_PUSH(env);
    PrimTable prims = get_prims();
    for (size_t i = 0; i < prims.count; i++) {
        Value* sym = intern(prims.prims[i].name);
        Value* primv = value_prim_create(prims.prims[i].name, prims.prims[i].fn);
        env_add_binding(env, sym, primv);
    }
    Value* true_sym = intern("#t");
    env_add_binding(env, true_sym, true_sym);
    GC_POP();
    return env;
}
