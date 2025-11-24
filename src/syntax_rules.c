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

typedef struct {
    Value* binding_list;
} CaptureMap;

typedef struct {
    Value* head;
    Value* tail;
} ListBuilder;

static bool pattern_match(Value* pattern, Value* input, Value* literals, CaptureMap* captures);
static Value* transcribe(Value* template, CaptureMap* captures);

static bool is_ellipsis_symbol(Value* sym)
{
    return sym->type == VALUE_SYMBOL && strcmp(sym->u.symbol, "...") == 0;
}

static bool is_wildcard_symbol(Value* sym)
{
    return sym->type == VALUE_SYMBOL && strcmp(sym->u.symbol, "_") == 0;
}

static bool is_literal_symbol(Value* sym, Value* literal_list)
{
    for (Value* cursor = literal_list; cursor != NIL; cursor = CDR(cursor)) {
        if (CAR(cursor) == sym)
            return true;
    }
    return false;
}

static void list_builder_init(ListBuilder* builder)
{
    builder->head = NIL;
    builder->tail = NIL;
}

static void list_builder_add(ListBuilder* builder, Value* item)
{
    Value* new_pair = CONS(item, NIL);

    if (builder->head == NIL) {
        builder->head = new_pair;
        builder->tail = new_pair;
    } else {
        CDR(builder->tail) = new_pair;
        builder->tail = new_pair;
    }
}

static void list_builder_set_dotted_tail(ListBuilder* builder, Value* tail_value)
{
    if (builder->head == NIL) {
        builder->head = tail_value;
    } else {
        CDR(builder->tail) = tail_value;
    }
}

static void captures_init(CaptureMap* captures)
{
    captures->binding_list = NIL;
}

static void captures_add(CaptureMap* captures, Value* var, Value* value)
{
    Value* binding = CONS(var, value);
    captures->binding_list = CONS(binding, captures->binding_list);
}

static Value* captures_lookup(CaptureMap* captures, Value* var)
{
    for (Value* cursor = captures->binding_list; cursor != NIL; cursor = CDR(cursor)) {
        Value* binding = CAR(cursor);
        if (CAR(binding) == var)
            return CDR(binding);
    }
    return NULL;
}

static void captures_append_repetition(CaptureMap* captures, Value* var, Value* value)
{
    Value* existing_values = captures_lookup(captures, var);

    if (!existing_values) {
        captures_add(captures, var, CONS(value, NIL));
        return;
    }

    for (Value* cursor = captures->binding_list; cursor != NIL; cursor = CDR(cursor)) {
        Value* binding = CAR(cursor);
        if (CAR(binding) == var) {
            CDR(binding) = CONS(value, CDR(binding));
            return;
        }
    }
}

static void captures_reverse_groups(CaptureMap* captures, Value* vars_to_reverse)
{
    for (Value* v_iter = vars_to_reverse; v_iter != NIL; v_iter = CDR(v_iter)) {
        Value* var = CAR(v_iter);
        for (Value* c_iter = captures->binding_list; c_iter != NIL; c_iter = CDR(c_iter)) {
            Value* binding = CAR(c_iter);
            if (CAR(binding) == var) {
                CDR(binding) = list_reverse(CDR(binding));
                break;
            }
        }
    }
}

static Value* collect_pattern_variables(Value* pattern, Value* literals)
{
    if (pattern->type == VALUE_SYMBOL) {
        if (is_ellipsis_symbol(pattern) || is_wildcard_symbol(pattern)) {
            return NIL;
        }
        if (!is_literal_symbol(pattern, literals)) {
            return CONS(pattern, NIL);
        }
        return NIL;
    }

    if (pattern->type == VALUE_PAIR) {
        Value* head_vars = collect_pattern_variables(CAR(pattern), literals);
        GC_PUSH(head_vars);
        Value* tail_vars = collect_pattern_variables(CDR(pattern), literals);
        Value* all_vars = list_append(head_vars, tail_vars);
        GC_POP();
        return all_vars;
    }

    return NIL;
}

static bool match_ellipsis_sequence(
    Value* repeated_pattern,
    Value* tail_pattern,
    Value* input_list,
    Value* literals,
    CaptureMap* captures)
{
    int input_length = list_length(input_list);
    int tail_length = list_length(tail_pattern);
    int repetition_count = input_length - tail_length;

    if (repetition_count < 0)
        return false;

    Value* repeated_vars = collect_pattern_variables(repeated_pattern, literals);
    GC_PUSH(repeated_vars);

    for (Value* v = repeated_vars; v != NIL; v = CDR(v)) {
        captures_add(captures, CAR(v), NIL);
    }

    Value* input_cursor = input_list;
    for (int i = 0; i < repetition_count; ++i) {
        CaptureMap local_captures;
        captures_init(&local_captures);
        GC_PUSH(local_captures.binding_list);

        if (!pattern_match(repeated_pattern, CAR(input_cursor), literals, &local_captures)) {
            GC_POP();
            GC_POP();
            return false;
        }

        for (Value* v = repeated_vars; v != NIL; v = CDR(v)) {
            Value* var = CAR(v);
            Value* val = captures_lookup(&local_captures, var);
            if (val) {
                captures_append_repetition(captures, var, val);
            }
        }

        GC_POP();
        input_cursor = CDR(input_cursor);
    }

    captures_reverse_groups(captures, repeated_vars);
    GC_POP();

    return pattern_match(tail_pattern, input_cursor, literals, captures);
}

static bool pattern_match_list(
    Value* pattern_list,
    Value* input_list,
    Value* literals,
    CaptureMap* captures)
{
    Value* pattern_cursor = pattern_list;
    Value* input_cursor = input_list;

    while (pattern_cursor != NIL && pattern_cursor->type == VALUE_PAIR) {

        if (CDR(pattern_cursor) != NIL && CDR(pattern_cursor)->type == VALUE_PAIR && is_ellipsis_symbol(CAR(CDR(pattern_cursor)))) {
            Value* repeated_pattern = CAR(pattern_cursor);
            Value* tail_pattern = CDR(CDR(pattern_cursor));
            return match_ellipsis_sequence(
                repeated_pattern, tail_pattern, input_cursor, literals, captures);
        }

        if (input_cursor == NIL || input_cursor->type != VALUE_PAIR)
            return false;

        if (!pattern_match(CAR(pattern_cursor), CAR(input_cursor), literals, captures)) {
            return false;
        }

        pattern_cursor = CDR(pattern_cursor);
        input_cursor = CDR(input_cursor);
    }

    return pattern_match(pattern_cursor, input_cursor, literals, captures);
}

static bool pattern_match(
    Value* pattern,
    Value* input,
    Value* literals,
    CaptureMap* captures)
{
    if (pattern->type == VALUE_SYMBOL) {

        if (is_literal_symbol(pattern, literals)) {
            return input->type == VALUE_SYMBOL && pattern == input;
        }

        if (is_wildcard_symbol(pattern))
            return true;

        captures_add(captures, pattern, input);
        return true;
    }

    if (pattern->type == VALUE_PAIR) {
        return pattern_match_list(pattern, input, literals, captures);
    }

    return value_equal(pattern, input);
}

static int calculate_repetition_depth(Value* vars, CaptureMap* captures)
{
    int depth = 0;
    bool depth_initialized = false;

    for (Value* v = vars; v != NIL; v = CDR(v)) {
        Value* bound_list = captures_lookup(captures, CAR(v));
        if (!bound_list)
            continue;

        int len = list_length(bound_list);

        if (!depth_initialized) {
            depth = len;
            depth_initialized = true;
        } else if (depth != len) {
            return -1;
        }
    }

    return depth;
}

static void transcribe_ellipsis(
    Value* repeated_template,
    CaptureMap* captures,
    ListBuilder* builder)
{
    Value* vars = collect_pattern_variables(repeated_template, NIL);
    GC_PUSH(vars);

    int count = calculate_repetition_depth(vars, captures);
    if (count < 0) {
        runtime_error("syntax-rules: mismatched repetition lengths in template");
        count = 0;
    }

    for (int i = 0; i < count; ++i) {
        CaptureMap local_captures;
        captures_init(&local_captures);
        GC_PUSH(local_captures.binding_list);

        for (Value* v = vars; v != NIL; v = CDR(v)) {
            Value* full_list = captures_lookup(captures, CAR(v));
            if (!full_list) {
                continue;
            }
            Value* item = list_ref(full_list, i);
            if (!item) {
                continue;
            }
            captures_add(&local_captures, CAR(v), item);
        }

        Value* expanded = transcribe(repeated_template, &local_captures);
        GC_PUSH(expanded);
        list_builder_add(builder, expanded);
        GC_POP();
        GC_POP();
    }

    GC_POP();
}

static Value* transcribe_list(Value* template_list, CaptureMap* captures)
{
    ListBuilder builder;
    list_builder_init(&builder);
    GC_PUSH(builder.head);

    Value* cursor = template_list;
    while (cursor != NIL && cursor->type == VALUE_PAIR) {

        if (CDR(cursor) != NIL && CDR(cursor)->type == VALUE_PAIR && is_ellipsis_symbol(CAR(CDR(cursor)))) {
            transcribe_ellipsis(CAR(cursor), captures, &builder);
            cursor = CDR(CDR(cursor));
        } else {
            Value* val = transcribe(CAR(cursor), captures);
            GC_PUSH(val);
            list_builder_add(&builder, val);
            GC_POP();
            cursor = CDR(cursor);
        }
    }

    if (cursor != NIL) {
        Value* dotted_tail = transcribe(cursor, captures);
        list_builder_set_dotted_tail(&builder, dotted_tail);
    }

    GC_POP();
    return builder.head;
}

static Value* transcribe(Value* template, CaptureMap* captures)
{
    if (template->type == VALUE_SYMBOL) {
        Value* val = captures_lookup(captures, template);
        return val ? val : template;
    }

    if (template->type == VALUE_PAIR) {
        return transcribe_list(template, captures);
    }

    return template;
}

Value* expand_syntax_rules(Value* macro_value, Value* input_expr)
{
    SyntaxRules* rules = macro_value->u.syntax_rules;
    Value* input_args = CDR(input_expr);

    for (size_t i = 0; i < rules->rule_count; ++i) {
        CaptureMap captures;
        captures_init(&captures);
        GC_PUSH(captures.binding_list);

        if (pattern_match(rules->rules[i].pattern,
                input_args,
                rules->literals,
                &captures)) {
            Value* result = transcribe(rules->rules[i].template, &captures);
            GC_POP();
            return result;
        }

        GC_POP();
    }

    return runtime_error("syntax error: no matching pattern for macro");
}

static SyntaxRules* syntax_rules_alloc(Value* literals, Value* env, unsigned int count)
{
    SyntaxRules* rules = xmalloc(sizeof(SyntaxRules));

    rules->literals = literals;
    rules->defining_env = env;
    rules->rule_count = count;
    rules->rules = xmalloc(count * sizeof(SyntaxRule));

    return rules;
}

Value* parse_define_syntax(Value* expr, Value* env)
{
    if (list_length(expr) != 3)
        return runtime_error("define-syntax: bad form (name syntax-rules)");

    Value* name = CADR(expr);
    Value* rules_form = CADDR(expr);

    if (rules_form->type != VALUE_PAIR || CAR(rules_form) != intern("syntax-rules")) {
        return runtime_error("define-syntax: expected syntax-rules");
    }

    Value* literals = CADR(rules_form);
    Value* rules_list = CDDR(rules_form);
    size_t rule_count = list_length(rules_list);

    SyntaxRules* rules = syntax_rules_alloc(literals, env, rule_count);
    Value* rules_obj = value_syntax_rules_create(rules);
    GC_PUSH(rules_obj);

    Value* cursor = rules_list;
    for (size_t i = 0; i < rule_count; ++i, cursor = CDR(cursor)) {

        Value* rule = CAR(cursor);
        if (list_length(rule) != 2) {
            GC_POP();
            return runtime_error("syntax-rules: rule must be in form (pattern template)");
        }

        if (CAR(rule)->type != VALUE_PAIR) {
            GC_POP();
            return runtime_error("syntax-rules: invalid pattern");
        }

        rules->rules[i].pattern = CDR(CAR(rule));
        rules->rules[i].template = CADR(rule);
    }

    env_add_binding(env, name, rules_obj);
    GC_POP();
    return name;
}

void syntax_rules_free(SyntaxRules* rules)
{
    if (rules) {
        free(rules->rules);
        free(rules);
    }
}
