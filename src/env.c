#include "env.h"
#include "gc.h"
#include "pair.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>

static Value* lookup_in_frame(Value* frame, Value* sym)

{
    for (Value* pair = frame; pair != NIL; pair = CDR(pair)) {
        Value* binding = CAR(pair);
        if (!CAR(binding) || !CAR(binding)->u.symbol || !sym->u.symbol) {
            return NULL;
        }
        if (strcmp(CAR(binding)->u.symbol, sym->u.symbol) == 0) {
            return CDR(binding);
        }
    }
    return NULL;
}

Value* env_lookup(Value* env, Value* sym)
{
    for (Value* frame = env; frame != NIL; frame = CDR(frame)) {
        Value* result = lookup_in_frame(CAR(frame), sym);
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
    CAR(env) = CONS(binding, CAR(env));
    GC_POP();
}

Value* env_extend(Value* env, Value* params, Value* args)
{
    Value* frame = NIL;
    GC_PUSH(frame);

    while (params != NIL && params->type == VALUE_PAIR) {
        if (args == NIL) {
            fprintf(stderr, "Error: not enough arguments provided\n");
            break;
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
