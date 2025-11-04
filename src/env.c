#include "env.h"
#include "gc.h"
#include "pair.h"

#include <stddef.h>

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
        frame = CONS(CONS(CAR(params), CAR(args)), frame);
        params = CDR(params);
        args = CDR(args);
    }
    GC_POP();
    return CONS(frame, env);
}
