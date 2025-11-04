
#pragma once

#include "value.h"

void gc_init(void);
void gc_destroy(void);
void gc_collect(void);

void gc_register_root(Value** addr);

void gc_push_root(Value** addr);
void gc_pop_root(void);

#define GC_PUSH(var) gc_push_root(&(var))
#define GC_POP() gc_pop_root()

Value* gc_alloc(ValueType type);
