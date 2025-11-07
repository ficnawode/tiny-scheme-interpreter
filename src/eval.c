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

static Value* expand_quasiquote(Value* expr, Value* env);

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

static EvalResult handle_quasiquote(Value* expr, Value* env)
{
    Value* form = CADR(expr);
    Value* expanded_form = expand_quasiquote(form, env);
    return result_value(expanded_form);
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

static EvalResult handle_load_file(Value* expr, Value* env)
{
    Value* filename_expr = CADR(expr);
    if (filename_expr == NIL) {
        fprintf(stderr, "load: expected a filename\n");
        return result_value(NIL);
    }

    Value* filename_val = eval(filename_expr, env);
    if (filename_val->type != VALUE_STRING) {
        fprintf(stderr, "load: argument must be a string\n");
        return result_value(NIL);
    }

    Value* result = eval_file(filename_val->u.string, env);
    return result_value(result);
}

static EvalResult handle_define_macro(Value* expr, Value* env)
{
    Value* target = CADR(expr);
    Value* name = CAR(target);
    Value* params = CDR(target);
    Value* body = CDR(CDR(expr));

    Value* macro = value_macro_create(params, body, env);
    env_add_binding(env, name, macro);
    return result_value(name);
}

typedef EvalResult (*SpecialFormFn)(Value*, Value*);

typedef struct {
    const char* name;
    SpecialFormFn fn;
} SpecialFormDef;

static EvalResult try_handle_special_form(Value* expr, Value* env)
{
    static SpecialFormDef special_forms[] = {
        { "quote", handle_quote },
        { "quasiquote", handle_quasiquote },
        { "if", handle_if },
        { "define", handle_define },
        { "lambda", handle_lambda },
        { "load", handle_load_file },
        { "define-macro", handle_define_macro },
    };
    static const size_t special_forms_count = sizeof(special_forms) / sizeof(*special_forms);

    Value* op = CAR(expr);
    if (!op || op->type != VALUE_SYMBOL)
        return result_no_match();

    for (size_t i = 0; i < special_forms_count; i++) {
        if (value_is_symbol_name(op, special_forms[i].name))
            return special_forms[i].fn(expr, env);
    }

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
    case VALUE_MACRO:
        fprintf(stderr, "Interpreter error: macro object reached apply phase.\n");
        return result_value(NIL);
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

static Value* expand_unquote(Value* form, Value* expanded_rest, Value* env)
{
    Value* arg = eval(CADR(form), env);
    GC_PUSH(arg);
    Value* result = CONS(arg, expanded_rest);
    GC_POP();
    return result;
}

static Value* expand_unquote_splicing(Value* form, Value* expanded_rest, Value* env)
{
    Value* arg_list = eval(CADR(form), env);

    if (arg_list->type != VALUE_PAIR && arg_list != NIL) {
        fprintf(stderr, "unquote-splicing: expected a list\n");
        return NIL;
    }

    GC_PUSH(arg_list);

    Value* head = NIL;
    Value* tail = NIL;

    for (Value* p = arg_list; p != NIL; p = CDR(p)) {
        Value* node = CONS(CAR(p), NIL);
        if (head == NIL) {
            head = tail = node;
        } else {
            CDR(tail) = node;
            tail = node;
        }
    }

    if (tail != NIL)
        CDR(tail) = expanded_rest;
    else
        head = expanded_rest;

    GC_POP();
    return head;
}

static Value* expand_quasiquote_element(Value* item, Value* expanded_rest, Value* env)
{
    Value* expanded_item = expand_quasiquote(item, env);
    GC_PUSH(expanded_item);
    Value* result = CONS(expanded_item, expanded_rest);
    GC_POP();
    return result;
}

static Value* expand_quasiquote_list(Value* list, Value* env)
{
    if (list == NIL)
        return NIL;

    Value* item = CAR(list);
    Value* rest = CDR(list);

    GC_PUSH(list);
    Value* expanded_rest = expand_quasiquote_list(rest, env);
    GC_PUSH(expanded_rest);

    Value* result;

    if (item->type == VALUE_PAIR && CAR(item)->type == VALUE_SYMBOL) {
        if (value_is_symbol_name(CAR(item), "unquote")) {
            result = expand_unquote(item, expanded_rest, env);
        } else if (value_is_symbol_name(CAR(item), "unquote-splicing")) {
            result = expand_unquote_splicing(item, expanded_rest, env);
        } else {
            result = expand_quasiquote_element(item, expanded_rest, env);
        }
    } else {
        result = expand_quasiquote_element(item, expanded_rest, env);
    }

    GC_POP();
    GC_POP();
    return result;
}

static Value* expand_quasiquote(Value* expr, Value* env)
{
    if (expr->type != VALUE_PAIR) {
        return expr;
    }

    if (CAR(expr)->type == VALUE_SYMBOL) {
        if (value_is_symbol_name(CAR(expr), "unquote")) {
            return eval(CADR(expr), env);
        }
        if (value_is_symbol_name(CAR(expr), "unquote-splicing")) {
            fprintf(stderr, "unquote-splicing not valid at top-level\n");
            return NIL;
        }
    }

    return expand_quasiquote_list(expr, env);
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

Value* macro_expand(Value* expr, Value* env);

static Value* expand_macro_call(Value* macro, Value* full_expr)
{
    Value* macro_params = macro->u.macro.params;
    Value* macro_body = macro->u.macro.body;
    Value* captured_env = macro->u.macro.env;

    Value* arg_list = CONS(full_expr, NIL);
    GC_PUSH(arg_list);

    Value* expansion_env = env_extend(captured_env, macro_params, arg_list);
    GC_POP();
    GC_PUSH(expansion_env);

    Value* result = NIL;
    GC_PUSH(result);
    for (Value* p = macro_body; p != NIL; p = CDR(p)) {
        result = eval(CAR(p), expansion_env);
    }

    GC_POP();
    GC_POP();
    return result;
}

Value* expand_macro(Value* expr, Value* env)
{
    for (int i = 0; i < 100; i++) {
        if (expr->type != VALUE_PAIR) {
            return expr;
        }

        Value* op = CAR(expr);
        if (op->type != VALUE_SYMBOL) {
            return expr;
        }

        Value* macro_obj = env_lookup(env, op);
        if (!macro_obj || macro_obj->type != VALUE_MACRO) {
            return expr;
        }

        expr = expand_macro_call(macro_obj, expr);
    }

    fprintf(stderr, "Macro expansion limit exceeded; check for infinite recursion in macro.\n");
    return NIL;
}

Value* eval(Value* expr, Value* env)
{
    for (;;) {
        expr = expand_macro(expr, env);

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
