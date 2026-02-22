#include "parser.h"
#include "error.h"
#include "gc.h"
#include "intern.h"
#include "pair.h"
#include "util.h"

#include <stdio.h>
#include <ctype.h>
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

static SchemeNum string_to_number(const char* s) {
    int len = strlen(s);
    if (len == 0) return fixnum_create(0);

    // Complex literals
    if (s[len - 1] == 'i') {
        if (len == 1) return complex_create(fixnum_create(0), fixnum_create(1));
        if (len == 2 && s[0] == '+') return complex_create(fixnum_create(0), fixnum_create(1));
        if (len == 2 && s[0] == '-') return complex_create(fixnum_create(0), fixnum_create(-1));

        char* buf = xstrdup(s);
        buf[len - 1] = '\0';
        
        // Split real and imag at the last + or - (that isn't at the start)
        int split = -1;
        for (int i = len - 2; i > 0; i--) {
            if (buf[i] == '+' || buf[i] == '-') { split = i; break; }
        }

        if (split != -1) {
            char real_str[128], imag_str[128];
            strncpy(real_str, buf, split); real_str[split] = '\0';
            strcpy(imag_str, buf + split);
            
            // Handle case like 3+i (imag part is just "+")
            SchemeNum r = string_to_number(real_str);
            SchemeNum i;
            if (strcmp(imag_str, "+") == 0) i = fixnum_create(1);
            else if (strcmp(imag_str, "-") == 0) i = fixnum_create(-1);
            else i = string_to_number(imag_str);

            free(buf);
            return complex_create(r, i);
        } else {
            // Pure imaginary (e.g., "5i")
            SchemeNum i = string_to_number(buf);
            free(buf);
            return complex_create(fixnum_create(0), i);
        }
    }

    // Rational literals
    const char* slash = strchr(s, '/');
    if (slash) {
        char* buf = xstrdup(s);
        buf[slash - s] = '\0';
        SchemeNum n = string_to_number(buf);
        SchemeNum d = string_to_number(slash + 1);
        free(buf);
        return rational_create(n, d);
    }

    // Floating Point literals
    if (strchr(s, '.')) {
        return floatnum_create(strtod(s, NULL));
    }

    return fixnum_create(atol(s));
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
        SchemeNum sn = string_to_number(p->current.lexeme);
        parser_advance(p);
        return value_num_create(sn);
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
