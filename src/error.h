#pragma once
#include "lexer.h"
#include "value.h"

void print_parser_error(Location loc, const char* source_code, const char* format, ...);

Value* runtime_error(const char* fmt, ...);
void print_runtime_error(Value* err);

void debug_call_stack_register(void);
void debug_call_stack_push(Value* val);
void debug_call_stack_pop(void);
int debug_call_stack_length(void);
Value* debug_call_stack_get(void);
void debug_call_stack_set(Value* cs);
