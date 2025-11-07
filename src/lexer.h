#pragma once

typedef struct {
    int line;
    int col;
} Position;

typedef struct {
    Position start;
    Position end;
} Location;

typedef enum {
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_QUOTE,
    TOK_SYMBOL,
    TOK_STRING,
    TOK_NUMBER,
    TOK_EOF,
    TOK_ERROR
} TokenType;

typedef struct {
    TokenType type;
    char* lexeme;
    Location loc;
} Token;

typedef struct {
    struct {
        const char* data;
        int index;
    } buffer;
    Position cursor;
    char* lexeme;
} Lexer;

Lexer* lexer_create(const char* source);
void lexer_cleanup(Lexer* ctx);
Token lexer_next(Lexer* ctx);

void token_cleanup(Token* t);
