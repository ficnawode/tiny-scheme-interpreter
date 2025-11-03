#include "parser.h"
#include "intern.h"
#include "pair.h"
#include "util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void parser_advance(Parser* ctx)
{
    token_cleanup(&ctx->current);
    ctx->current = lexer_next(ctx->lexer);
}

Parser* parser_create(const char* source)
{
    Parser* ctx = xmalloc(sizeof(Parser));
    memset(ctx, 0, sizeof(Parser));
    ctx->lexer = lexer_create(source);
    parser_advance(ctx);
    return ctx;
}

void parser_cleanup(Parser* ctx)
{
    lexer_cleanup(ctx->lexer);
    free(ctx);
}

static Value* parse_list(Parser* p)
{
    if (p->current.type == TOK_RPAREN) {
        parser_advance(p);
        return NIL;
    }

    Value* head = NULL;
    Value* tail = NULL;

    for (;;) {
        Value* expr = parse_expr(p);
        if (!expr) {
            return NULL;
        }

        Value* node = CONS(expr, NIL);
        if (!head)
            head = tail = node;
        else
            tail = CDR(tail) = node;

        if (p->current.type == TOK_RPAREN) {
            parser_advance(p);
            return head;
        }

        if (p->current.type == TOK_EOF) {
            fprintf(stderr, "Syntax error: missing ')'\n");
            return NULL;
        }
    }
}

static Value* parse_quote(Parser* p)
{
    parser_advance(p);
    return CONS(intern("quote"), CONS(parse_expr(p), NIL));
}

Value* parse_expr(Parser* p)
{
    switch (p->current.type) {
    case TOK_LPAREN:
        parser_advance(p);
        if (p->current.type == TOK_RPAREN) {
            parser_advance(p);
            return NIL;
        }
        return parse_list(p);

    case TOK_QUOTE:
        return parse_quote(p);

    case TOK_NUMBER: {
        long n = atol(p->current.lexeme);
        parser_advance(p);
        return value_int_create(n);
    }

    case TOK_SYMBOL: {
        Value* sym = intern(p->current.lexeme);
        parser_advance(p);
        return sym;
    }

    case TOK_RPAREN:
        fprintf(stderr, "Unexpected ')'\n");
        return NIL;

    case TOK_EOF:
        return NULL;

    default:
        fprintf(stderr, "Unknown token\n");
        return NIL;
    }
}

Value* parse_from_string(const char* s)
{
    Parser* p = parser_create(s);
    Value* res = parse_expr(p);
    parser_cleanup(p);
    return res;
}
