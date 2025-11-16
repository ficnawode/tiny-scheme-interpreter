#pragma once
#include "lexer.h"
#include "value.h"

void print_parser_error(Location loc, const char* source_code, const char* format, ...);

Value* runtime_error(const char* fmt, ...);
void print_runtime_error(Value* err);

void register_call_stack(void);
void call_stack_push(Value* val);
void call_stack_pop(void);
void call_stack_clear(void);
int call_stack_length(void);
