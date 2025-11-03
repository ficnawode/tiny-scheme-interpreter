#pragma once
#include <stddef.h>

char* read_file(const char* path);
void* xmalloc(size_t n);
char* copy_substr(const char* start, int len);
char* xstrdup(const char* str);
