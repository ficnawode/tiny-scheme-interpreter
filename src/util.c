#include "util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* read_file(const char* path)
{
    FILE* file = fopen(path, "rb");
    if (!file) {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        return NULL;
    }

    fseek(file, 0L, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    char* buffer = xmalloc(file_size + 1);
    size_t bytes_read = fread(buffer, sizeof(char), file_size, file);

    if (bytes_read < file_size) {
        fprintf(stderr, "Could not read entire file \"%s\".\n", path);
        free(buffer);
        fclose(file);
        return NULL;
    }

    buffer[bytes_read] = '\0';
    fclose(file);
    return buffer;
}

void* xmalloc(size_t n)
{
    void* p = malloc(n);
    if (!p) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return p;
}

void* xrealloc(void* obj, size_t n)
{
    void* p = realloc(obj, n);
    if (!p) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return p;
}

char* xstrdup(const char* str)
{
    char* res = strdup(str);
    if (!res) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return res;
}

char* copy_substr(const char* start, int len)
{
    char* out = xmalloc(len + 1);
    memcpy(out, start, len);
    out[len] = '\0';
    return out;
}
