#include "parser.h"
#include "gc.h"
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
    token_cleanup(&ctx->current);
    lexer_cleanup(ctx->lexer);
    free(ctx);
}

static Value* parse_list_dotted_tail(Parser* p, Value* head, Value* tail)
{
    if (head == NIL) {
        fprintf(stderr, "Syntax error: dot operator in invalid context\n");
        return NULL;
    }
    parser_advance(p);

    Value* cdr_val = parse_expr(p);
    if (!cdr_val) {
        return NULL;
    }

    if (p->current.type != TOK_RPAREN) {
        fprintf(stderr, "Syntax error: expected ')' after dotted pair\n");
        return NULL;
    }

    parser_advance(p);
    CDR(tail) = cdr_val;
    return head;
}

static Value* parse_list(Parser* p)
{
    if (p->current.type == TOK_RPAREN) {
        parser_advance(p);
        return NIL;
    }

    Value* head = NIL;
    Value* tail = NIL;

    GC_PUSH(head);
    GC_PUSH(tail);

    for (;;) {
        if (p->current.type == TOK_RPAREN) {
            parser_advance(p);
            GC_POP();
            GC_POP();
            return head;
        }

        if (p->current.type == TOK_DOT) {
            head = parse_list_dotted_tail(p, head, tail);
            GC_POP();
            GC_POP();
            return head;
        }

        Value* expr = parse_expr(p);
        if (!expr) {
            GC_POP();
            GC_POP();
            return NULL;
        }

        GC_PUSH(expr);
        Value* node = CONS(expr, NIL);
        GC_POP();

        if (head == NIL) {
            head = node;
            tail = node;
        } else {
            CDR(tail) = node;
            tail = node;
        }

        if (p->current.type == TOK_EOF) {
            fprintf(stderr, "Syntax error: missing ')'\n");
            GC_POP();
            GC_POP();
            return NULL;
        }
    }
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
