#pragma once
#include "value.h"

Value* make_global_env(void);
Value* eval(Value* expr, Value* env);
Value* eval_file(const char* filename, Value* env);
Value* apply(Value* proc, Value* args);
bool is_evaluator_special_form(const char* name);
