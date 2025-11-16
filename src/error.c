#include "error.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

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
