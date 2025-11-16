#include "error.h"
#include "eval.h"
#include "gc.h"
#include "pair.h"
#include "util.h"
#include "value.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Value* call_stack = NULL;

void print_parser_error(Location loc, const char* source_code, const char* format, ...)
{
    if (!source_code || !format)
        return;

    int line = loc.start.line;
    int col = loc.start.col;
    int len = (loc.end.col > loc.start.col) ? (loc.end.col - loc.start.col) : 1;

    printf("Parser Error [%s::(%d,%d)]: ", loc.filename, line, col);

    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);
    putchar('\n');

    const char* line_start = source_code;
    int current_line = 1;

    while (current_line < line && *line_start) {
        if (*line_start++ == '\n')
            current_line++;
    }

    if (current_line == line) {
        const char* p = line_start;
        while (*p && *p != '\n')
            putchar(*p++);
        putchar('\n');
    }

    static const char underlines[] = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
                                     "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
                                     "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
    printf("%*s%.*s\n\n", col - 1, "", len, underlines);
}

void register_call_stack(void)
{
    gc_register_root(&call_stack);
    call_stack = NIL;
}

void call_stack_push(Value* expr)
{
    call_stack = CONS(expr, call_stack);
}

void call_stack_pop(void)
{
    call_stack = CDR(call_stack);
}

Value* runtime_error(const char* fmt, ...)
{
    va_list args1;
    va_start(args1, fmt);
    int size = vsnprintf(NULL, 0, fmt, args1);
    va_end(args1);

    if (size < 0) {
        Value* err = value_error_create("Failed to format runtime error message.");
        err->u.error.call_stack = call_stack;
        return err;
    }

    char* message_buffer = xmalloc(size + 1);

    va_list args2;
    va_start(args2, fmt);
    vsnprintf(message_buffer, size + 1, fmt, args2);
    va_end(args2);

    Value* err = value_error_create(message_buffer);
    err->u.error.call_stack = call_stack;

    free(message_buffer);

    return err;
}

void print_runtime_error(Value* err)
{
    fprintf(stderr, "Runtime Error: %s\n", err->u.error.message);
    if (err->u.error.call_stack != NIL) {
        fprintf(stderr, "Stack trace:\n");
        Value* trace = err->u.error.call_stack;
        for (; trace != NIL; trace = CDR(trace)) {
            fprintf(stderr, "  ");
            value_print(CAR(trace));
            fprintf(stderr, "\n");
        }
    }
}
