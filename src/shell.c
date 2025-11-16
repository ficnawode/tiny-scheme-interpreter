#include "shell.h"
#include "eval.h"
#include "gc.h"
#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFSIZE 4096

#ifdef USE_READLINE
#include <readline/history.h>
#include <readline/readline.h>
#endif

static char* read_input(void)
{
#ifdef USE_READLINE
    char* line = readline("=> ");
    if (line && *line) {
        add_history(line);
    }
    return line;
#else
    char buffer[BUFSIZE];

    printf("=> ");
    fflush(stdout);

    if (!fgets(buffer, sizeof(buffer), stdin)) {
        return NULL;
    }

    size_t len = strlen(buffer);
    if (len > 0 && buffer[len - 1] == '\n')
        buffer[len - 1] = '\0';

    return strdup(buffer);
#endif
}

int repl(Value* env)
{
    printf("Minimal Scheme in C. Ctrl-D to exit.\n");

    for (;;) {
        char* line = read_input();
        if (!line)
            break;

        Value* expr = parse_from_string(line);
        free(line);

        if (!expr) {
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
