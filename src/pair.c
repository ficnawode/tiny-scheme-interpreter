#include "pair.h"
#include "gc.h"

int list_length(Value* args)
{
    int n = 0;
    while (args != NIL && args->type == VALUE_PAIR) {
        n++;
        args = CDR(args);
    }
    return n;
}

Value* list_reverse(Value* list)
{
    Value* prev = NIL;
    Value* current = list;
    while (current != NIL) {
        Value* next = CDR(current);
        CDR(current) = prev;
        prev = current;
        current = next;
    }
    return prev;
}

Value* list_append(Value* a, Value* b)
{
    GC_PUSH(a);
    GC_PUSH(b);
    if (a == NIL) {
        GC_POP();
        GC_POP();
        return b;
    }

    Value* result = NIL;
    Value* tail = NIL;

    GC_PUSH(result);
    for (Value* p = a; p != NIL && p->type == VALUE_PAIR; p = CDR(p)) {
        Value* new_pair = value_cons_create(CAR(p), NIL);

        if (result == NIL) {
            result = new_pair;
            tail = new_pair;
        } else {
            CDR(tail) = new_pair;
            tail = new_pair;
        }
    }

    if (tail != NIL) {
        CDR(tail) = b;
    } else {
        result = b;
    }
    GC_POP();
    GC_POP();
    GC_POP();

    return result;
}

Value* list_ref(Value* list, int index)
{
    Value* p = list;
    for (int i = 0; i < index; ++i) {
        if (p == NIL || p->type != VALUE_PAIR)
            return NIL;
        p = CDR(p);
    }
    if (p == NIL || p->type != VALUE_PAIR)
        return NIL;
    return CAR(p);
}
