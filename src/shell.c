#include "shell.h"
#include "eval.h"
#include "gc.h"
#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <readline/history.h>
#include <readline/readline.h>

#define BUFSIZE 4096

int repl(Value* env)
{
    printf("Minimal Scheme in C. Ctrl-D to exit.\n");

    for (;;) {
        char* line = readline("=> ");
        if (line == NULL)
            break;

        if (*line) {
            add_history(line);
        }

        Value* expr = parse_from_string(line);

        free(line);

        if (!expr) {
            printf("Syntax error\n");
            continue;
        }

        GC_PUSH(expr);
        Value* res = eval(expr, env);
        GC_POP();
        value_print(res);
        printf("\n");
    }

    return 0;
}
