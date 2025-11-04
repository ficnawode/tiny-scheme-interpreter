#include "gc.h"
#include "util.h"

#include <stdio.h>
#include <stdlib.h>

typedef struct GCHeader {
    struct GCHeader* next;
    unsigned char marked;
} GCHeader;

static GCHeader* all_objects = NULL;

static size_t total_allocated_bytes = 0;
static size_t gc_threshold = 500 * 1024;

typedef struct RootNode {
    struct RootNode* next;
    Value** addr;
} RootNode;

static RootNode* global_roots = NULL;

static Value*** root_stack = NULL;
static size_t root_stack_size = 0;
static size_t root_stack_capacity = 0;

static inline GCHeader* header_of(Value* v)
{
    return (GCHeader*)(((char*)v) - sizeof(GCHeader));
}

void gc_init(void)
{
    root_stack_capacity = 128;
    root_stack = xmalloc(root_stack_capacity * sizeof(Value**));
}

void gc_register_root(Value** addr)
{
    RootNode* r = xmalloc(sizeof(RootNode));
    r->addr = addr;
    r->next = global_roots;
    global_roots = r;
}

void gc_push_root(Value** addr)
{
    if (root_stack_size == root_stack_capacity) {
        root_stack_capacity *= 2;
        root_stack = xrealloc(root_stack, root_stack_capacity * sizeof(Value**));
    }
    root_stack[root_stack_size++] = addr;
}

void gc_pop_root(void)
{
    root_stack_size--;
}

Value* gc_alloc(ValueType type)
{
    if (total_allocated_bytes >= gc_threshold) {
        gc_collect();
    }

    GCHeader* h = xmalloc(sizeof(GCHeader) + sizeof(Value));
    h->marked = 0;
    h->next = all_objects;
    all_objects = h;

    total_allocated_bytes += sizeof(GCHeader) + sizeof(Value);

    Value* v = (Value*)(h + 1);
    v->type = type;
    return v;
}

static void mark(Value* v)
{
    if (!v || v->type == VALUE_NIL)
        return;
    GCHeader* h = header_of(v);
    if (h->marked)
        return;
    h->marked = 1;

    switch (v->type) {
    case VALUE_PAIR:
        mark(v->u.pair.car);
        mark(v->u.pair.cdr);
        break;
    case VALUE_CLOSURE:
        mark(v->u.closure.params);
        mark(v->u.closure.body);
        mark(v->u.closure.env);
        break;
    default:
        break;
    }
}

static void sweep(void)
{
    size_t bytes_in_use = 0;
    GCHeader** p = &all_objects;
    while (*p) {
        GCHeader* h = *p;
        Value* v = (Value*)(h + 1);

        if (!h->marked) {
            *p = h->next;
            if (v->type == VALUE_SYMBOL)
                free(v->u.symbol);
            free(h);
        } else {
            h->marked = 0;
            bytes_in_use += sizeof(GCHeader) + sizeof(Value);
            p = &h->next;
        }
    }
    total_allocated_bytes = bytes_in_use;
}

void gc_collect(void)
{
    for (RootNode* r = global_roots; r; r = r->next)
        mark(*(r->addr));

    for (size_t i = 0; i < root_stack_size; ++i)
        mark(*(root_stack[i]));

    sweep();
}

void gc_destroy(void)
{
    GCHeader* current = all_objects;
    while (current) {
        GCHeader* next = current->next;

        Value* v = (Value*)(current + 1);

        if (v->type == VALUE_SYMBOL) {
            free(v->u.symbol);
        }

        free(current);

        current = next;
    }

    RootNode* r = global_roots;
    while (r) {
        RootNode* next = r->next;
        free(r);
        r = next;
    }

    free(root_stack);
}
