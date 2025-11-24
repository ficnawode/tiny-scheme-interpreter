#include "parser.h"
#include "error.h"
#include "gc.h"
#include "intern.h"
#include "pair.h"
#include "util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PRINT_ERROR_HERE(p, fmt, ...)        \
    do {                                     \
        print_parser_error((p)->current.loc, \
            (p)->lexer->buffer.data,         \
            fmt, ##__VA_ARGS__);             \
    } while (0)

void parser_advance(Parser* ctx)
{
    token_cleanup(&ctx->current);
    ctx->current = lexer_next(ctx->lexer);
}

Parser* parser_create(const char* source, const char* filename)
{
    Parser* ctx = xmalloc(sizeof(Parser));
    memset(ctx, 0, sizeof(Parser));
    ctx->lexer = lexer_create(source, filename);
    parser_advance(ctx);
    return ctx;
}

void parser_cleanup(Parser* ctx)
{
    token_cleanup(&ctx->current);
    lexer_cleanup(ctx->lexer);
    free(ctx);
}

static Value* read_list_items(Parser* p, Value** out_rev)
{
    Value* rev = NIL;
    GC_PUSH(rev);

    while (p->current.type != TOK_RPAREN && p->current.type != TOK_DOT) {

        Value* expr = parse_expr(p);
        if (!expr) {
            PRINT_ERROR_HERE(p, "encountered null expr while parsing list");
            GC_POP();
            return NULL;
        }

        GC_PUSH(expr);
        rev = CONS(expr, rev);
        GC_POP();

        if (p->current.type == TOK_EOF) {
            PRINT_ERROR_HERE(p, "missing ')'");
            GC_POP();
            return NULL;
        }
    }

    *out_rev = rev;
    GC_POP();
    return rev;
}

static Value* finish_proper_list(Value* rev, Parser* p)
{
    parser_advance(p);
    return list_reverse(rev);
}

static Value* finish_dotted_list(Value* rev, Parser* p)
{
    parser_advance(p);

    if (rev == NIL) {
        PRINT_ERROR_HERE(p, "dot in invalid context");
        return NULL;
    }

    Value* tail = parse_expr(p);
    if (!tail) {
        PRINT_ERROR_HERE(p, "expecting an expression");
        return NULL;
    }

    if (p->current.type != TOK_RPAREN) {
        PRINT_ERROR_HERE(p, "expected ')' after dotted pair");
        return NULL;
    }
    parser_advance(p);

    Value* result = list_reverse(rev);
    GC_PUSH(result);

    Value* last = result;
    while (CDR(last) != NIL)
        last = CDR(last);
    CDR(last) = tail;

    GC_POP();
    return result;
}

static Value* parse_list(Parser* p)
{
    if (p->current.type == TOK_RPAREN) {
        parser_advance(p);
        return NIL;
    }

    Value* rev = NIL;
    if (!read_list_items(p, &rev)) {
        return NULL;
    }

    if (p->current.type == TOK_RPAREN) {
        return finish_proper_list(rev, p);
    }

    return finish_dotted_list(rev, p);
}

static Value* list_to_vector(Value* list)
{
    int len = list_length(list);
    Value* vec = value_vector_create(len, NULL);

    Value* p = list;
    for (int i = 0; i < len; i++) {
        vec->u.vector.data[i] = CAR(p);
        p = CDR(p);
    }
    return vec;
}

static Value* parse_vector(Parser* p)
{
    parser_advance(p);

    if (p->current.type == TOK_RPAREN) {
        parser_advance(p);
        return value_vector_create(0, NULL);
    }

    Value* rev_items = NIL;
    if (!read_list_items(p, &rev_items)) {
        return NULL;
    }

    if (p->current.type == TOK_DOT) {
        PRINT_ERROR_HERE(p, "dot '.' not allowed in vector literals");
        return NULL;
    }

    if (p->current.type != TOK_RPAREN) {
        PRINT_ERROR_HERE(p, "expected ')' to close vector");
        return NULL;
    }
    parser_advance(p);

    Value* proper_list = list_reverse(rev_items);
    GC_PUSH(proper_list);
    Value* vec = list_to_vector(proper_list);
    GC_POP();
    return vec;
}

static Value* wrap_with_symbol(const char* sym_name, Parser* p)
{
    parser_advance(p);

    Value* sym = intern(sym_name);
    GC_PUSH(sym);

    Value* expr = parse_expr(p);
    GC_PUSH(expr);

    Value* result = CONS(sym, CONS(expr, NIL));

    GC_POP();
    GC_POP();
    return result;
}

Value* parse_expr(Parser* p)
{
    switch (p->current.type) {
    case TOK_LPAREN:
        parser_advance(p);
        return parse_list(p);
    case TOK_HASH:
        parser_advance(p);
        if (p->current.type != TOK_LPAREN) {
            PRINT_ERROR_HERE(p, "Expected '(' after '#' for vector literal");
            return NULL;
        }
        return parse_vector(p);
    case TOK_QUOTE:
        return wrap_with_symbol("quote", p);
    case TOK_QUASIQUOTE:
        return wrap_with_symbol("quasiquote", p);
    case TOK_UNQUOTE:
        return wrap_with_symbol("unquote", p);
    case TOK_UNQUOTE_SPLICING:
        return wrap_with_symbol("unquote-splicing", p);
    case TOK_NUMBER:
        long n = atol(p->current.lexeme);
        parser_advance(p);
        return value_int_create(n);
    case TOK_SYMBOL:
        Value* sym = intern(p->current.lexeme);
        parser_advance(p);
        return sym;
    case TOK_STRING:
        Value* str = value_string_create(p->current.lexeme);
        parser_advance(p);
        return str;
    case TOK_RPAREN:
        PRINT_ERROR_HERE(p, "Unexpected ')'");
        return NULL;
    case TOK_EOF:
        return NULL;
    case TOK_ERROR:
        PRINT_ERROR_HERE(p, "Token Error: %s", p->current.lexeme);
        return NULL;
    default:
        PRINT_ERROR_HERE(p, "Unknown token: %s", p->current.lexeme);
        return NULL;
    }
}

Value* parse_from_string(const char* s)
{
    Parser* p = parser_create(s, "<repl>");
    Value* res = parse_expr(p);
    parser_cleanup(p);
    return res;
}
