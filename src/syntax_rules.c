#include "syntax_rules.h"
#include "env.h"
#include "error.h"
#include "eval.h"
#include "gc.h"
#include "intern.h"
#include "pair.h"
#include "util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static bool syntax_match(Value* pattern, Value* expr, Value* literals, Value** menv);
static Value* instantiate(Value* tpl, Value* menv);

static bool is_literal(Value* sym, Value* literals)
{
    for (Value* p = literals; p != NIL; p = CDR(p)) {
        if (CAR(p) == sym) {
            return true;
        }
    }
    return false;
}

static void menv_add_binding(Value** menv, Value* var, Value* val)
{
    Value* binding = CONS(var, val);
    GC_PUSH(binding);
    *menv = CONS(binding, *menv);
    GC_POP();
}

static Value* menv_lookup(Value* menv, Value* var)
{
    for (Value* p = menv; p != NIL; p = CDR(p)) {
        Value* binding = CAR(p);
        if (CAR(binding) == var) {
            return CDR(binding);
        }
    }
    return NULL;
}

static void menv_add_ellipsis_binding(Value** menv, Value* var, Value* val)
{
    Value* existing = menv_lookup(*menv, var);
    if (existing == NULL) {
        menv_add_binding(menv, var, CONS(val, NIL));
    } else {
        Value* binding = CONS(val, existing);
        for (Value* p = *menv; p != NIL; p = CDR(p)) {
            Value* b = CAR(p);
            if (CAR(b) == var) {
                CDR(b) = binding;
                return;
            }
        }
    }
}

static Value* get_pattern_vars(Value* pattern, Value* literals)
{
    if (pattern->type == VALUE_SYMBOL) {
        const char* sym = pattern->u.symbol;
        if (strcmp(sym, "...") == 0 || strcmp(sym, "_") == 0) {
            return NIL;
        }
        if (!is_literal(pattern, literals)) {
            return CONS(pattern, NIL);
        }
    }
    if (pattern->type == VALUE_PAIR) {
        Value* vars_car = get_pattern_vars(CAR(pattern), literals);
        GC_PUSH(vars_car);
        Value* vars_cdr = get_pattern_vars(CDR(pattern), literals);
        GC_PUSH(vars_cdr);
        Value* result = list_append(vars_car, vars_cdr);
        GC_POP();
        GC_POP();
        return result;
    }
    return NIL;
}

static bool syntax_match_list(Value* pattern, Value* expr, Value* literals, Value** menv)
{
    Value* ellipsis_sym = intern("...");

    Value* p_temp = pattern;
    Value* repeatable_pattern = NULL;
    int pre_ellipsis_len = 0;
    while (p_temp != NIL && p_temp->type == VALUE_PAIR) {
        if (CDR(p_temp) != NIL && CDR(p_temp)->type == VALUE_PAIR && CAR(CDR(p_temp)) == ellipsis_sym) {
            repeatable_pattern = CAR(p_temp);
            break;
        }
        pre_ellipsis_len++;
        p_temp = CDR(p_temp);
    }

    if (!repeatable_pattern) {
        Value* p_head = pattern;
        Value* e_head = expr;

        while (p_head->type == VALUE_PAIR) {
            if (e_head == NIL || e_head->type != VALUE_PAIR) {
                return false;
            }
            if (!syntax_match(CAR(p_head), CAR(e_head), literals, menv))
                return false;
            p_head = CDR(p_head);
            e_head = CDR(e_head);
        }

        return syntax_match(p_head, e_head, literals, menv);
    }

    Value* p_head = pattern;
    Value* e_head = expr;
    for (int i = 0; i < pre_ellipsis_len; ++i) {
        if (e_head == NIL || e_head->type != VALUE_PAIR) {
            return false;
        }

        if (!syntax_match(CAR(p_head), CAR(e_head), literals, menv)) {
            return false;
        }
        p_head = CDR(p_head);
        e_head = CDR(e_head);
    }

    Value* post_ellipsis_pattern = CDR(CDR(p_head));
    int post_len = list_length(post_ellipsis_pattern);
    int expr_rem_len = list_length(e_head);
    int repeat_count = expr_rem_len - post_len;

    if (repeat_count < 0)
        return false;

    Value* repeating_vars = get_pattern_vars(repeatable_pattern, literals);
    GC_PUSH(repeating_vars);
    for (Value* v = repeating_vars; v != NIL; v = CDR(v)) {
        menv_add_binding(menv, CAR(v), NIL);
    }

    Value* repeatable_exprs = e_head;
    for (int i = 0; i < repeat_count; ++i) {
        Value* sub_menv = NIL;
        GC_PUSH(sub_menv);
        if (!syntax_match(repeatable_pattern, CAR(repeatable_exprs), literals, &sub_menv)) {
            GC_POP();
            GC_POP();
            return false;
        }

        for (Value* v = repeating_vars; v != NIL; v = CDR(v)) {
            Value* var = CAR(v);
            Value* val = menv_lookup(sub_menv, var);
            if (val) {
                menv_add_ellipsis_binding(menv, var, val);
            }
        }

        GC_POP();
        repeatable_exprs = CDR(repeatable_exprs);
    }

    for (Value* v = repeating_vars; v != NIL; v = CDR(v)) {
        Value* var = CAR(v);
        for (Value* p = *menv; p != NIL; p = CDR(p)) {
            Value* binding = CAR(p);
            if (CAR(binding) == var) {
                CDR(binding) = list_reverse(CDR(binding));
                break;
            }
        }
    }
    GC_POP();
    return syntax_match(post_ellipsis_pattern, repeatable_exprs, literals, menv);
}

static bool syntax_match(Value* pattern, Value* expr, Value* literals, Value** menv)
{
    if (pattern->type == VALUE_SYMBOL) {
        if (is_literal(pattern, literals)) {
            return expr->type == VALUE_SYMBOL && pattern == expr;
        }
        if (pattern == intern("_")) {
            return true;
        }
        menv_add_binding(menv, pattern, expr);
        return true;
    }

    if (pattern->type == VALUE_PAIR) {
        return syntax_match_list(pattern, expr, literals, menv);
    }

    if (!value_equal(pattern, expr)) {
        return false;
    }
    return true;
}

static void append_to_result(Value** head, Value** tail, Value* item)
{
    Value* new_pair = CONS(item, NIL);

    if (*head == NIL) {
        *head = new_pair;
        *tail = new_pair;
    } else {
        CDR(*tail) = new_pair;
        *tail = new_pair;
    }
}

static Value* instantiate_list(Value* tpl, Value* menv)
{
    Value* result_head = NIL;
    Value* result_tail = NIL;
    Value* ellipsis_sym = intern("...");

    GC_PUSH(result_head);

    Value* p = tpl;
    while (p != NIL && p->type == VALUE_PAIR) {

        bool is_ellipsis = false;
        if (CDR(p) != NIL && CDR(p)->type == VALUE_PAIR) {
            if (CAR(CDR(p)) == ellipsis_sym) {
                is_ellipsis = true;
            }
        }

        if (is_ellipsis) {
            Value* item_template = CAR(p);

            Value* pattern_vars = get_pattern_vars(item_template, NIL);
            GC_PUSH(pattern_vars);

            int repeat_count = 0;
            bool found_var = false;

            for (Value* v = pattern_vars; v != NIL; v = CDR(v)) {
                Value* var_sym = CAR(v);
                Value* binding = menv_lookup(menv, var_sym);

                if (binding) {
                    int len = list_length(binding);

                    if (!found_var) {
                        repeat_count = len;
                        found_var = true;
                    } else {
                        if (len != repeat_count) {
                            return runtime_error("Mismatch in ellipsis repetition lengths");
                        }
                    }
                }
            }

            for (int i = 0; i < repeat_count; ++i) {
                Value* sub_menv = NIL;
                GC_PUSH(sub_menv);

                for (Value* v = pattern_vars; v != NIL; v = CDR(v)) {
                    Value* var_sym = CAR(v);
                    Value* all_matches = menv_lookup(menv, var_sym);
                    if (all_matches) {
                        Value* ith_val = list_ref(all_matches, i);
                        if (ith_val) {
                            menv_add_binding(&sub_menv, var_sym, ith_val);
                        }
                    }
                }

                Value* expanded_item = instantiate(item_template, sub_menv);
                GC_PUSH(expanded_item);
                append_to_result(&result_head, &result_tail, expanded_item);
                GC_POP();
                GC_POP();
            }

            GC_POP();

            p = CDR(CDR(p));

        } else {
            Value* val = instantiate(CAR(p), menv);
            GC_PUSH(val);
            append_to_result(&result_head, &result_tail, val);
            GC_POP();
            p = CDR(p);
        }
    }

    if (p != NIL) {
        Value* val = instantiate(p, menv);
        if (result_head == NIL) {
            result_head = val;
        } else {
            CDR(result_tail) = val;
        }
    }

    GC_POP();
    return result_head;
}

static Value* instantiate(Value* tpl, Value* menv)
{
    if (tpl->type == VALUE_SYMBOL) {
        Value* val = menv_lookup(menv, tpl);
        if (val) {
            return val;
        }
        return tpl;
    }

    if (tpl->type == VALUE_PAIR) {
        return instantiate_list(tpl, menv);
    }

    // Vector will go here

    return tpl;
}

Value* expand_syntax_rules(Value* macro_val, Value* expr)
{
    SyntaxRules* sr = macro_val->u.syntax_rules;
    Value* expr_no_keyword = CDR(expr);

    for (size_t i = 0; i < sr->rule_count; ++i) {
        Value* menv = NIL;
        GC_PUSH(menv);

        if (syntax_match(sr->rules[i].pattern, expr_no_keyword, sr->literals, &menv)) {
            Value* result = instantiate(sr->rules[i].template, menv);
            GC_POP();
            return result;
        }

        GC_POP();
    }
    return runtime_error("Syntax error: no matching pattern for macro");
}

Value* parse_define_syntax(Value* expr, Value* env)
{
    if (list_length(expr) != 3) {
        return runtime_error("Invalid define-syntax form: requires a name and a syntax-rules form");
    }

    Value* name = CADR(expr);
    Value* syntax_rules_form = CADDR(expr);

    if (syntax_rules_form->type != VALUE_PAIR || CAR(syntax_rules_form)->type != VALUE_SYMBOL || CAR(syntax_rules_form) != intern("syntax-rules")) {
        return runtime_error("Expected syntax-rules in define-syntax");
    }

    Value* literals = CADR(syntax_rules_form);
    Value* rules_list = CDDR(syntax_rules_form);
    size_t rule_count = list_length(rules_list);

    SyntaxRules* sr = xmalloc(sizeof(SyntaxRules));
    sr->literals = literals;
    sr->defining_env = env;
    sr->rule_count = rule_count;
    sr->rules = xmalloc(rule_count * sizeof(SyntaxRule));

    Value* v = value_syntax_rules_create(sr);
    GC_PUSH(v);

    Value* p = rules_list;
    for (size_t i = 0; i < rule_count; ++i, p = CDR(p)) {
        Value* rule_form = CAR(p);
        if (list_length(rule_form) != 2) {
            GC_POP();
            return runtime_error("Invalid syntax rule: must be a pair of (pattern template)");
        }

        Value* pattern_with_keyword = CAR(rule_form);
        if (pattern_with_keyword == NIL || pattern_with_keyword->type != VALUE_PAIR) {
            GC_POP();
            return runtime_error("Invalid syntax-rules pattern: must be a list starting with the keyword.");
        }

        sr->rules[i].pattern = CDR(pattern_with_keyword);
        sr->rules[i].template = CADR(rule_form);
    }

    env_add_binding(env, name, v);
    GC_POP();
    return name;
}

void syntax_rules_free(SyntaxRules* sr)
{
    free(sr->rules);
    free(sr);
}
