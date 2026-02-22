#include "eval.h"
#include "env.h"
#include "error.h"
#include "gc.h"
#include "intern.h"
#include "lexer.h"
#include "pair.h"
#include "parser.h"
#include "prims.h"
#include "syntax_rules.h"
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
    EvalResult r = { .kind = RES_VALUE, .v.value = value };
    return r;
}

static inline EvalResult result_tail(Value* expr, Value* env)
{
    EvalResult r = { .kind = RES_TAILCALL, .tc.expr = expr, .tc.env = env };
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

    Value* reversed_args = NIL;
    GC_PUSH(list);
    GC_PUSH(reversed_args);

    for (Value* p = list; p != NIL; p = CDR(p)) {
        Value* evaluated_arg = eval(CAR(p), env);
        GC_PUSH(evaluated_arg);
        reversed_args = CONS(evaluated_arg, reversed_args);
        GC_POP();
    }

    GC_POP();
    GC_POP();
    return list_reverse(reversed_args);
}

static EvalResult handle_quote(Value* expr, Value* env)
{
    (void)env;
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

static EvalResult handle_define(Value* expr, Value* env)
{
    Value* target = CADR(expr);
    Value* value_expr = CDR(CDR(expr));

    Value* sym_to_bind;
    Value* val_to_eval;

    if (target->type == VALUE_SYMBOL) {
        sym_to_bind = target;
        val_to_eval = CAR(value_expr);
    } else if (target->type == VALUE_PAIR) {
        sym_to_bind = CAR(target);
        Value* params = CDR(target);
        Value* body = value_expr;
        val_to_eval = value_closure_create(params, body, env);
    } else {
        return result_value(runtime_error("Invalid define syntax"));
    }

    GC_PUSH(val_to_eval);
    Value* result = eval(val_to_eval, env);
    GC_POP();

    GC_PUSH(result);
    env_add_binding(env, sym_to_bind, result);
    GC_POP();

    return result_value(sym_to_bind);
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
    if (!filename_expr || filename_expr == NIL) {
        return result_value(runtime_error("\"load\" expected a filename"));
    }

    Value* filename_val = eval(filename_expr, env);
    if (filename_val->type != VALUE_STRING) {
        return result_value(runtime_error("\"load\" argument must be a string"));
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

static EvalResult handle_begin(Value* expr, Value* env)
{
    Value* body = CDR(expr);
    if (body == NIL) {
        return result_value(NIL);
    }

    while (CDR(body) != NIL) {
        eval(CAR(body), env);
        body = CDR(body);
    }

    return result_tail(CAR(body), env);
}

static Value* find_binding(Value* env, const char* symbol)
{
    for (Value* frame = env; frame != NIL; frame = CDR(frame)) {
        for (Value* pair = CAR(frame); pair != NIL; pair = CDR(pair)) {
            Value* binding = CAR(pair);
            Value* name = CAR(binding);
            if (name->type == VALUE_SYMBOL && name->u.symbol != NULL && strcmp(name->u.symbol, symbol) == 0) {
                return binding;
            }
        }
    }
    return NULL;
}

static EvalResult handle_set_bang(Value* expr, Value* env)
{
    if (list_length(CDR(expr)) != 2) {
        return result_value(runtime_error("bad set! syntax. Expected (set! <symbol> <value>)"));
    }

    Value* target = CADR(expr);

    if (target->type != VALUE_SYMBOL) {
        return result_value(runtime_error("first argument of set! must be a symbol, but got something else"));
    }

    Value* value_expr = CADDR(expr);
    Value* value = eval(value_expr, env);

    if (value && value->type == VALUE_ERROR) {
        return result_value(value);
    }

    Value* binding = find_binding(env, target->u.symbol);
    if (binding == NULL) {
        return result_value(runtime_error("set! failed - symbol '%s' is not bound", target->u.symbol));
    }

    CDR(binding) = value;
    return result_value(value);
}

static EvalResult apply_proc(Value* proc, Value* args);

static EvalResult handle_apply(Value* expr, Value* env)
{
    Value* args_unevaluated = CDR(expr);

    if (list_length(args_unevaluated) < 2) {
        return result_value(runtime_error("apply expects at least 2 arguments"));
    }

    Value* apply_args_evaluated = eval_args(args_unevaluated, env);
    GC_PUSH(apply_args_evaluated);

    Value* proc_to_apply = CAR(apply_args_evaluated);

    Value* reversed_prefix_args = NIL;
    GC_PUSH(reversed_prefix_args);

    Value* p;
    for (p = CDR(apply_args_evaluated); CDR(p) != NIL; p = CDR(p)) {
        reversed_prefix_args = CONS(CAR(p), reversed_prefix_args);
    }

    Value* final_arg_list = list_reverse(reversed_prefix_args);
    GC_POP();
    GC_PUSH(final_arg_list);

    Value* last_arg_list = CAR(p);
    if (last_arg_list->type != VALUE_PAIR && last_arg_list != NIL) {
        GC_POP();
        GC_POP();
        return result_value(runtime_error("last argument of apply must be a list"));
    }

    if (final_arg_list == NIL) {
        final_arg_list = last_arg_list;
    } else {
        Value* tail = final_arg_list;
        while (CDR(tail) != NIL) {
            tail = CDR(tail);
        }
        CDR(tail) = last_arg_list;
    }

    EvalResult result = apply_proc(proc_to_apply, final_arg_list);

    GC_POP();
    GC_POP();
    return result;
}

static EvalResult handle_define_syntax(Value* expr, Value* env)
{
    return result_value(parse_define_syntax(expr, env));
}

typedef EvalResult (*SpecialFormFn)(Value*, Value*);

typedef struct {
    const char* name;
    SpecialFormFn fn;
} SpecialFormDef;
static SpecialFormDef special_forms[] = {
    { "quote", handle_quote },
    { "quasiquote", handle_quasiquote },
    { "if", handle_if },
    { "define", handle_define },
    { "lambda", handle_lambda },
    { "load", handle_load_file },
    { "define-macro", handle_define_macro },
    { "define-syntax", handle_define_syntax },
    { "begin", handle_begin },
    { "set!", handle_set_bang },
    { "apply", handle_apply },
};
static const size_t special_forms_count = sizeof(special_forms) / sizeof(*special_forms);

bool is_evaluator_special_form(const char* name)
{
    for (size_t i = 0; i < special_forms_count; i++) {
        if (strcmp(name, special_forms[i].name) == 0) {
            return true;
        }
    }
    return false;
}

static EvalResult try_handle_special_form(Value* expr, Value* env)
{
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
    GC_PUSH(proc);
    Value* closure_env = env_extend(proc->u.closure.env, proc->u.closure.params, args);
    GC_PUSH(closure_env);

    Value* body = proc->u.closure.body;

    while (CDR(body) != NIL) {
        eval(CAR(body), closure_env);
        body = CDR(body);
    }

    Value* next_expr = CAR(body);
    EvalResult r = result_tail(next_expr, closure_env);

    GC_POP();
    GC_POP();
    return r;
}

static EvalResult apply_proc(Value* proc, Value* args)
{
    switch (proc->type) {
    case VALUE_PRIMITIVE:
        return result_value(apply_primitive(proc, args));
    case VALUE_CLOSURE:
        return apply_closure(proc, args);
    case VALUE_MACRO:
        return result_value(runtime_error("macro object reached apply phase."));
    default:
        return result_value(runtime_error("attempt to call non-function"));
    }
}

static EvalResult eval_pair(Value* expr, Value* env)
{
    EvalResult sf = try_handle_special_form(expr, env);
    if (sf.kind != RES_NO_MATCH) {
        return sf;
    }

    Value* op = CAR(expr);
    Value* proc = NULL;
    GC_PUSH(proc);
    proc = eval(op, env);

    if (proc->type == VALUE_ERROR) {
        GC_POP();
        return result_value(proc);
    }

    Value* args = NULL;
    GC_PUSH(args);
    args = eval_args(CDR(expr), env);

    debug_call_stack_push(expr);
    EvalResult r = apply_proc(proc, args);

    GC_POP();
    GC_POP();
    return r;
}

static Value* eval_symbol(Value* expr, Value* env)
{
    Value* v = env_lookup(env, expr);
    if (!v) {
        return runtime_error("Unbound variable - %s", expr->u.symbol);
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
    GC_PUSH(arg_list);

    if (arg_list->type != VALUE_PAIR && arg_list != NIL) {
        GC_POP();
        return runtime_error("unquote-splicing expects a list");
    }

    Value* head = NIL;
    Value* tail = NIL;
    GC_PUSH(head);
    GC_PUSH(tail);

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
    GC_POP();
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
    if (list == NULL) {
        return runtime_error("received NULL list during quasiquote list expansion");
    }
    if (list->type != VALUE_PAIR || list == NIL) {
        return list;
    }

    Value* item = CAR(list);
    Value* rest = CDR(list);

    GC_PUSH(list);
    GC_PUSH(rest);

    Value* expanded_rest = expand_quasiquote_list(rest, env);
    GC_PUSH(expanded_rest);

    Value* result;

    if (item && item->type == VALUE_PAIR && CAR(item) && CAR(item)->type == VALUE_SYMBOL) {
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
    GC_POP();
    return result;
}

static Value* expand_quasiquote(Value* expr, Value* env)
{
    if (expr == NULL) {
        return NIL;
    }

    if (expr->type != VALUE_PAIR) {
        return expr;
    }

    if (CAR(expr) && CAR(expr)->type == VALUE_SYMBOL) {
        if (value_is_symbol_name(CAR(expr), "unquote")) {
            return eval(CADR(expr), env);
        }
        if (value_is_symbol_name(CAR(expr), "unquote-splicing")) {
            return runtime_error("unquote-splicing not valid at top level");
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
    case VALUE_NUM:
    case VALUE_NIL:
    case VALUE_PRIMITIVE:
    case VALUE_CLOSURE:
    case VALUE_STRING:
    case VALUE_ERROR:
        return result_value(expr);
    case VALUE_SYMBOL:
        return result_value(eval_symbol(expr, env));
    case VALUE_PAIR:
        return eval_pair(expr, env);
    default:
        return result_value(runtime_error("eval - unknown expression type"));
    }
}

static Value* expand_macro_call(Value* macro, Value* full_expr)
{
    Value* macro_params = macro->u.macro.params;
    GC_PUSH(macro_params);
    Value* macro_body = macro->u.macro.body;
    GC_PUSH(macro_body);
    Value* captured_env = macro->u.macro.env;
    Value* arg_list = CDR(full_expr);

    Value* expansion_env = env_extend(captured_env, macro_params, arg_list);
    GC_PUSH(expansion_env);

    Value* result = NIL;
    GC_PUSH(result);

    for (Value* p = macro_body; p != NIL; p = CDR(p)) {
        result = eval(CAR(p), expansion_env);
    }

    GC_POP();
    GC_POP();
    GC_POP();
    GC_POP();

    return result;
}

#define MAX_MACRO_RECURSION_DEPTH 100
static void print_macro_debug(const Value* expr, const Value* op, const Value* expansion)
{
    printf("Macro Expansion [%s]:\n  Input: ", op->u.symbol);
    value_print(expr);
    printf("\n  Output: ");
    if (expansion && expansion->type == VALUE_ERROR) {
        printf("ERROR: %s", expansion->u.error.message);
    } else {
        value_print(expansion);
    }
    printf("\n\n");
}

Value* expand_macro(Value* expr, Value* env)
{
    GC_PUSH(expr);
    for (int i = 0; i < MAX_MACRO_RECURSION_DEPTH; i++) {
        if (expr->type != VALUE_PAIR) {
            GC_POP();
            return expr;
        }

        Value* op = CAR(expr);
        if (op->type != VALUE_SYMBOL) {
            GC_POP();
            return expr;
        }

        Value* macro_obj = env_lookup(env, op);
        GC_PUSH(macro_obj);

        if (!macro_obj) {
            GC_POP();
            GC_POP();
            return expr;
        }

        Value* expansion = NULL;

        if (macro_obj->type == VALUE_MACRO) {
            expansion = expand_macro_call(macro_obj, expr);
        } else if (macro_obj->type == VALUE_SYNTAX_RULES) {
            expansion = expand_syntax_rules(macro_obj, expr);
        } else {
            GC_POP();
            GC_POP();
            return expr;
        }

        static const int DEBUG_MACRO = 0;
        if (DEBUG_MACRO) {
            print_macro_debug(expr, op, expansion);
        }

        GC_POP();
        GC_POP();
        expr = expansion;
        GC_PUSH(expr);
    }

    GC_POP();
    return runtime_error("Macro expansion limit (n=%d) exceeded, check for infinite recursion in macro", MAX_MACRO_RECURSION_DEPTH);
}

Value* eval(Value* expr, Value* env)
{
    Value* initial_stack = debug_call_stack_get();

    GC_PUSH(expr);
    GC_PUSH(env);

    for (;;) {
        expr = expand_macro(expr, env);
        if (expr && expr->type == VALUE_ERROR) {
            GC_POP();
            GC_POP();
            return expr;
        }

        EvalResult r = eval_dispatch(expr, env);

        if (r.kind == RES_VALUE) {
            debug_call_stack_set(initial_stack);
            GC_POP();
            GC_POP();
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
    Parser* p = parser_create(input, filename);
    Value* last_result = NIL;
    GC_PUSH(last_result);
    for (;;) {
        Value* expr = parse_expr(p);
        if (!expr) {
            break;
        }
        GC_PUSH(expr);
        last_result = eval(expr, env);
        GC_POP();

        if (last_result && last_result->type == VALUE_ERROR) {
            break;
        }
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
        GC_PUSH(sym);
        Value* primv = value_prim_create(prims.prims[i].name, prims.prims[i].fn);
        env_add_binding(env, sym, primv);
        GC_POP();
    }
    Value* true_sym = intern("#t");
    env_add_binding(env, true_sym, true_sym);
    Value* false_sym = intern("#f");
    env_add_binding(env, false_sym, false_sym);
    GC_POP();
    return env;
}
