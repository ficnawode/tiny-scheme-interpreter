#pragma once
#include "value.h"

#include <stddef.h>

typedef struct
{
    const char* name;
    PrimFn fn;
} PrimDef;

typedef struct
{
    PrimDef* prims;
    size_t count;
} PrimTable;

PrimTable get_prims(void);
