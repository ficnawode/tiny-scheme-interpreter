#include "pair.h"

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
