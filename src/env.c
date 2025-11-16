#include "env.h"
#include "error.h"
#include "gc.h"
#include "pair.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>

static Value* lookup_in_frame(Value* frame, Value* sym)
{
    if (!frame || frame == NIL)
        return NULL;
    if (!sym || sym == NIL || sym->type != VALUE_SYMBOL)
        return NULL;

    for (Value* pair = frame; pair != NIL; pair = CDR(pair)) {
        Value* binding = CAR(pair);
        if (!binding || binding == NIL) {
            continue;
        }

        Value* name = CAR(binding);
        if (!name || name == NIL || name->type != VALUE_SYMBOL) {
            continue;
        }

        if (name->u.symbol != NULL && sym->u.symbol != NULL && strcmp(name->u.symbol, sym->u.symbol) == 0) {
            return CDR(binding);
        }
    }
    return NULL;
}

Value* env_lookup(Value* env, Value* sym)
{
    for (Value* frame = env; frame != NIL; frame = CDR(frame)) {
        Value* current_frame = CAR(frame);
        Value* result = lookup_in_frame(current_frame, sym);
        if (result != NULL) {
            return result;
        }
    }
    return NULL;
}

void env_add_binding(Value* env, Value* sym, Value* val)
{
    Value* binding = CONS(sym, val);
    GC_PUSH(binding);
    GC_PUSH(env);

    CAR(env) = CONS(binding, CAR(env));
    GC_POP();
    GC_POP();
}

Value* env_extend(Value* env, Value* params, Value* args)
{
    Value* frame = NIL;
    GC_PUSH(frame);

    while (params != NIL && params->type == VALUE_PAIR) {
        if (args == NIL) {
            GC_POP();
            return runtime_error("Env extend - not enough arguments provided");
        }

        Value* binding = CONS(CAR(params), CAR(args));
        GC_PUSH(binding);
        frame = CONS(binding, frame);
        GC_POP();

        params = CDR(params);
        args = CDR(args);
    }

    if (params != NIL) {
        Value* binding = CONS(params, args);
        GC_PUSH(binding);
        frame = CONS(binding, frame);
        GC_POP();
    }

    Value* new_env = CONS(frame, env);

    GC_POP();

    return new_env;
}
