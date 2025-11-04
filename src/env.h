#pragma once
#include "value.h"

void env_add_binding(Value* env, Value* sym, Value* val);
Value* env_lookup(Value* env, Value* sym);
Value* env_extend(Value* env, Value* params, Value* args);
