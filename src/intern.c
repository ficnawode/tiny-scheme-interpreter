#include "intern.h"
#include "gc.h"
#include "pair.h"
#include "prims.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

static Value* intern_table = NULL;

static Value* intern_table_insert(const char* s)
{
    Value* sym = value_symbol_create(s);
    GC_PUSH(sym);
    intern_table = CONS(sym, intern_table);
    GC_POP();
    return sym;
}

Value* intern(const char* s)
{
    assert(intern_table && "Attempting to use uninitialized itern table! \n");
    for (Value* p = intern_table; p != NIL; p = CDR(p)) {
        Value* sym = CAR(p);
        if (strcmp(sym->u.symbol, s) == 0)
            return sym;
    }
    return intern_table_insert(s);
}

void intern_init(void)
{
    if (intern_table != NULL)
        return;

    intern_table = NIL;
    gc_register_root(&intern_table);

    intern("if");
    intern("define");
    intern("define-macro");
    intern("lambda");

    intern("quote");
    intern("quasiquote");
    intern("unquote");
    intern("unquote-splicing");

    intern("#t");

    PrimTable prim_table = get_prims();
    for (size_t i = 0; i < prim_table.count; i++) {
        intern(prim_table.prims[i].name);
    }
}
