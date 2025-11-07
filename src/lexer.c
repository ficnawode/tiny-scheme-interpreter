
#include "lexer.h"
#include "util.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

Lexer* lexer_create(const char* source)
{
    Lexer* ctx = xmalloc(sizeof(Lexer));

    ctx->buffer.data = source;
    assert(ctx->buffer.data && "Out of memory");
    ctx->buffer.index = 0;

    ctx->cursor.line = 1;
    ctx->cursor.col = 1;
    return ctx;
}

void lexer_cleanup(Lexer* ctx)
{
    free(ctx);
}

static char lexer_peek(Lexer* ctx)
{
    return ctx->buffer.data[ctx->buffer.index];
}

static void lexer_advance(Lexer* ctx)
{
    char c = lexer_peek(ctx);
    if (c == '\n') {
        ctx->cursor.line++;
        ctx->cursor.col = 1;
    } else {
        ctx->cursor.col++;
    }
    ctx->buffer.index++;
}
static void skip_to_next_line(Lexer* ctx)
{
    while (lexer_peek(ctx) != '\n' && lexer_peek(ctx) != '\0') {
        lexer_advance(ctx);
    }
}

static void skip_comments_and_whitespace(Lexer* ctx)
{
    for (;;) {
        char c = lexer_peek(ctx);

        if (isspace((unsigned char)c)) {
            lexer_advance(ctx);
        } else if (c == ';') {
            skip_to_next_line(ctx);
        } else {
            break;
        }
    }
}

static Token token_create(TokenType type, const char* lexeme,
    Position start, Position end)
{
    Token tok;
    tok.type = type;
    if (lexeme != NULL) {
        tok.lexeme = strdup(lexeme);
    } else {
        tok.lexeme = NULL;
    }
    tok.loc.start = start;
    tok.loc.end = end;
    return tok;
}

void token_cleanup(Token* t)
{
    if (t->lexeme) {
        free(t->lexeme);
        t->lexeme = NULL;
    }
}

static bool is_terminator(char c)
{
    return c == '\0' || isspace((unsigned char)c) || c == '(' || c == ')' || c == '\'' || c == '"';
}

static bool is_number_lexeme(const char* s)
{
    if (!s || *s == '\0')
        return false;
    if (s[0] == '-' && s[1] != '\0' && isdigit((unsigned char)s[1]))
        return true;
    return isdigit((unsigned char)s[0]);
}

static Token read_word(Lexer* ctx)
{
    Position start_pos = ctx->cursor;
    const char* start = &ctx->buffer.data[ctx->buffer.index];
    int start_index = ctx->buffer.index;

    while (!is_terminator(lexer_peek(ctx)))
        lexer_advance(ctx);

    int len = ctx->buffer.index - start_index;
    char* lex = copy_substr(start, len);

    TokenType type = is_number_lexeme(lex) ? TOK_NUMBER : TOK_SYMBOL;
    Position end_pos = ctx->cursor;

    Token tok = token_create(type, lex, start_pos, end_pos);
    free(lex);
    return tok;
}

#define BUFSIZE 1024
static Token read_string(Lexer* ctx)
{
    Position start_pos = ctx->cursor;
    lexer_advance(ctx);

    char buffer[BUFSIZE];
    int i = 0;
    for (;;) {
        char c = lexer_peek(ctx);

        if (c == '\0') {
            return token_create(TOK_ERROR, "Unterminated string literal",
                start_pos, ctx->cursor);
        }

        if (c == '"') {
            lexer_advance(ctx);
            break;
        }

        if (i >= BUFSIZE - 1) {
            return token_create(TOK_ERROR, "String literal too long",
                start_pos, ctx->cursor);
        }

        if (c == '\\') {
            lexer_advance(ctx);
            c = lexer_peek(ctx);

            switch (c) {
            case 'n':
                c = '\n';
                break;
            case 't':
                c = '\t';
                break;
            case '"':
                c = '"';
                break;
            case '\\':
                c = '\\';
                break;
            }
        }

        buffer[i++] = c;
        lexer_advance(ctx);
    }

    buffer[i] = '\0';
    return token_create(TOK_STRING, buffer, start_pos, ctx->cursor);
}

Token lexer_next(Lexer* ctx)
{
    skip_comments_and_whitespace(ctx);
    Position start_pos = ctx->cursor;
    char c = lexer_peek(ctx);

    switch (c) {
    case '(':
        lexer_advance(ctx);
        return token_create(TOK_LPAREN, "(", start_pos, ctx->cursor);
    case ')':
        lexer_advance(ctx);
        return token_create(TOK_RPAREN, ")", start_pos, ctx->cursor);
    case '\'':
        lexer_advance(ctx);
        return token_create(TOK_QUOTE, "'", start_pos, ctx->cursor);
    case '"':
        return read_string(ctx);
    case '\0':
        return token_create(TOK_EOF, NULL, start_pos, start_pos);
    default:
        return read_word(ctx);
    }
}
