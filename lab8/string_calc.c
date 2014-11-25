/* string_calc: a simple "calculator" on the language of strings.
 *
 * The syntax of the language is:
 *
 * E -> T E'
 * E' -> '.' T E'
 * E -> ε
 * T -> V '^' T
 * T -> V
 * V -> '(' E ')'
 * V -> STRING
 *
 * Where STRING matches the regular expression [0-9]+.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    intptr_t length;
    intptr_t capacity;
    char *data;
} charvec_t;


charvec_t *charvec_new() {
    charvec_t *temp = calloc(1, sizeof(charvec_t));
    if (temp == NULL) abort();

    char *data = calloc(8, 1);
    if (data == NULL) abort();

    temp->length = 0;
    temp->capacity = 8;
    temp->data = data;

    return temp;
}

void charvec_push(charvec_t *vec, char c) {
    if (vec->length < vec->capacity) {
        // pass
    } else {
        vec->capacity *= 2;
        vec->data = realloc(vec->data, vec->capacity);
        if (vec->data == NULL) abort();
    }
    vec->data[vec->length++] = c;
    vec->data[vec->length] = '\0';
}

void charvec_free(charvec_t *vec) {
    if (vec) {
        free(vec->data);
    }
}

typedef enum {
    CONCAT,
    REPEAT
} opkind_t;

typedef struct op_t {
    struct op_t *left;
    struct op_t *right;
    opkind_t op;
} op_t;

typedef struct {
    char tag;
    union {
        op_t op;
        char *val;
    };
} node_t;

typedef enum {
    STRING,
    DOT,
    LPAREN,
    RPAREN,
    CARET,
    TEOF
} token_type_t;

typedef struct {
    token_type_t type;
    charvec_t *val;
} token_t;

token_t lex() {
    token_t temp = {0};

    char c;
    int seen_char = false;
#define HANDLE do { if (seen_char) { ungetc(c, stdin); return temp; } } while (0)
    while (true) {
        switch ((c = getc(stdin))) {
            case '.':
                HANDLE;
                temp.type = DOT;
                return temp;
            case '^':
                HANDLE;
                temp.type = CARET;
                return temp;
            case '(':
                HANDLE;
                temp.type = LPAREN;
                return temp;
            case ')':
                HANDLE;
                temp.type = RPAREN;
                return temp;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                if (!seen_char) temp.val = charvec_new();
                seen_char = true;
                charvec_push(temp.val, c);
                break;
            case ' ':
            case '\t':
            case '\n':
            case '\r':
                HANDLE;
                break;
            case EOF:
                HANDLE;
                temp.type = TEOF;
                return temp;
            default:
                fprintf(stderr, "invalid character %c while lexing\n", c);
                abort();
                break;
        }
    }
#undef HANDLE
}

void token_free(token_t *tok) {
    charvec_free(tok->val);
}

void token_print(token_t *tok) {
    switch (tok->type) {
        case STRING:
            printf("{STRING:%s}", tok->val->data);
            break;
        case DOT:
            puts("{DOT}");
            break;
        case LPAREN:
            puts("{LPAREN}");
            break;
        case RPAREN:
            puts("{RPAREN}");
            break;
        case CARET:
            puts("{CARET}");
            break;
        case TEOF:
            puts("{TEOF}");
            break;
        default:
            abort();
            break;
    }
}

int main(int argc, char **argv) {
    while (true) {
        token_t tok = lex();
        token_print(&tok);
        if (tok.type == TEOF) break;
    }
}
