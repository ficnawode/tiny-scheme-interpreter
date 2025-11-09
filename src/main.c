#include "eval.h"
#include "gc.h"
#include "intern.h"
#include "shell.h"
#include "util.h"
#include "value.h"

#include <libgen.h>
#include <stdio.h>
#include <string.h>

static void load_stdlib(Value* env)
{
    char path_buf[1024];
    strncpy(path_buf, __FILE__, sizeof(path_buf));
    path_buf[sizeof(path_buf) - 1] = '\0';

    char* dir = dirname(path_buf);

    char stdlib_path[1024];
    snprintf(stdlib_path, sizeof(stdlib_path), "%s/stdlib.scm", dir);

    eval_file(stdlib_path, env);
}

int main(int argc, char** argv)
{
    if (argc > 2) {
        fprintf(stderr, "Usage: %s [filename]\n", argv[0]);
        return 1;
    }

    gc_init();
    intern_init();
    Value* global_env = make_global_env();
    gc_register_root(&global_env);
    load_stdlib(global_env);

    if (argc == 2) {
        Value* result = eval_file(argv[1], global_env);
        if (result) {
            value_print(result);
            printf("\n");
        }
    } else {
        repl(global_env);
    }
    gc_destroy();
    return 0;
}
