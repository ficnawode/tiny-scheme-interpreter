#include "gc.h"
#include "syntax_rules.h"
#include "util.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct GCObject {
    struct GCObject* next;
    unsigned char marked;
    Value value;
} GCObject;

static GCObject* all_objects = NULL;

#define container_of(ptr, type, member) ({                      \
    const typeof( ((type *)0)->member ) *__mptr = (ptr);    \
    (type *)( (char *)__mptr - offsetof(type, member) ); })

static inline GCObject* gc_object_of(Value* v)
{
    return container_of(v, GCObject, value);
}

static size_t total_allocated_bytes = 0;

#define GC_MIN_THRESHOLD 256 * 1024
static size_t gc_threshold = GC_MIN_THRESHOLD;

typedef struct RootNode {
    struct RootNode* next;
    Value** addr;
} RootNode;

static RootNode* global_roots = NULL;

static Value*** root_stack = NULL;
static size_t root_stack_size = 0;
static size_t root_stack_capacity = 0;

void gc_init(void)
{
    root_stack_capacity = 128;
    root_stack = xmalloc(root_stack_capacity * sizeof(Value**));
    root_stack_size = 0;
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
    if (root_stack_size == 0) {
        fprintf(stderr, "gc_pop_root(): underflow (popped more than pushed)\n");
        exit(1);
    }
    root_stack_size--;
    root_stack[root_stack_size] = NULL;
}

Value* gc_alloc(ValueType type)
{
    if (total_allocated_bytes >= gc_threshold) {
        gc_collect();
    }

    GCObject* obj = xmalloc(sizeof(GCObject));

    memset(&obj->value, 0, sizeof(Value));

    obj->marked = 0;
    obj->next = all_objects;
    all_objects = obj;

    total_allocated_bytes += sizeof(GCObject);

    Value* v = &obj->value;
    v->type = type;
    return v;
}

static void mark(Value* v)
{
    if (!v || v->type == VALUE_NIL)
        return;

    GCObject* obj = gc_object_of(v);
    if (obj->marked)
        return;
    obj->marked = 1;

    switch (v->type) {
    case VALUE_PAIR:
        mark(v->u.pair.car);
        mark(v->u.pair.cdr);
        break;
    case VALUE_VECTOR:
        for (size_t i = 0; i < v->u.vector.length; ++i) {
            mark(v->u.vector.data[i]);
        }
        break;
    case VALUE_CLOSURE:
        mark(v->u.closure.params);
        mark(v->u.closure.body);
        mark(v->u.closure.env);
        break;
    case VALUE_MACRO:
        mark(v->u.macro.params);
        mark(v->u.macro.body);
        mark(v->u.macro.env);
        break;
    case VALUE_SYNTAX_RULES:
        SyntaxRules* sr = v->u.syntax_rules;
        mark(sr->literals);
        mark(sr->defining_env);
        for (size_t i = 0; i < sr->rule_count; i++) {
            mark(sr->rules[i].pattern);
            mark(sr->rules[i].template);
        }
        break;
    case VALUE_ERROR:
        mark(v->u.error.call_stack);
    default:
        break;
    }
}

static void free_value_internals(Value* v)
{
    switch (v->type) {
    case VALUE_SYMBOL:
        free(v->u.symbol);
        return;
    case VALUE_STRING:
        free(v->u.string);
        return;
    case VALUE_VECTOR:
        if (v->u.vector.data) {
            free(v->u.vector.data);
        }
        break;
    case VALUE_ERROR:
        free(v->u.error.message);
        return;
    case VALUE_SYNTAX_RULES:
        syntax_rules_free(v->u.syntax_rules);
    default:
    }
}

static void free_gc_object(GCObject* o)
{
    free_value_internals(&o->value);
    memset(o, 0xDD, sizeof(GCObject));
    free(o);
}

static void sweep(void)
{
    size_t bytes_in_use = 0;
    GCObject** p = &all_objects;
    while (*p) {
        GCObject* current = *p;

        if (!current->marked) {
            *p = current->next;
            free_gc_object(current);
        } else {
            current->marked = 0;
            bytes_in_use += sizeof(GCObject);
            p = &current->next;
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

    gc_threshold = total_allocated_bytes * 2;
    if (gc_threshold < GC_MIN_THRESHOLD)
        gc_threshold = GC_MIN_THRESHOLD;
}

void gc_destroy(void)
{
    GCObject* current = all_objects;
    while (current) {
        GCObject* next = current->next;
        free_gc_object(current);
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
