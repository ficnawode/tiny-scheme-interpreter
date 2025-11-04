#include "env.h"
#include "gc.h"
#include "pair.h"

#include <stddef.h>
#include <string.h>

static Value* lookup_in_frame(Value* frame, Value* sym)

{
    for (Value* pair = frame; pair != NIL; pair = CDR(pair)) {
        Value* binding = CAR(pair);
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

    while (params != NIL) {
        Value* binding = CONS(CAR(params), CAR(args));
        GC_PUSH(binding);
        frame = CONS(binding, frame);
        GC_POP();
        params = CDR(params);
        args = CDR(args);
    }

    Value* new_env = CONS(frame, env);
    GC_PUSH(new_env);

    GC_POP();
    GC_POP();

    return new_env;
}
