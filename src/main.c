#include "eval.h"
#include "intern.h"
#include "shell.h"
#include "util.h"
#include "value.h"

#include <stdio.h>

int main(int argc, char** argv)
{
    if (argc > 2) {
        fprintf(stderr, "Usage: %s [filename]\n", argv[0]);
        return 1;
    }

    intern_init();
    Value* global_env = make_global_env();

    if (argc == 2) {
        Value* result = eval_file(argv[1], global_env);
        if (result) {
            value_print(result);
            printf("\n");
        }
    } else {
        repl(global_env);
    }

    value_free(global_env);
    intern_cleanup();
    return 0;
}
