/* string_calc.c: a simple "calculator" on the language of strings.
 *
 * To use this, compile with your favorite C compiler. I do it with:
 * `clang -O -o string_calc string_calc.c`. Then, run with `./string_calc`.
 * You can feed lines in, and the calculator will run when you press Ctrl-D
 * (or give it EOF), but due to the line-buffering nature of portable stdin,
 * you need to press enter and THEN Ctrl-D. A final Ctrl-D will end the
 * program if there's no input.
 *
 * The grammar this parser implements is:
 *
 * E -> T E'
 * E' -> '.' T E'
 * E' -> ε
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
    // 1 for the NUL
    if (vec->length < vec->capacity - 1) {
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
        free(vec);
    }
}

void charvec_concat(charvec_t *source, charvec_t *dest) {
    char *c = source->data;
    for (int i = 0; i < source->length; i++) {
        charvec_push(dest, *c++);
    }
}

charvec_t *charvec_from_cstr(char *str) {
    charvec_t *res = charvec_new();
    while (*str) {
        charvec_push(res, *str++);
    }
    return res;
}

charvec_t *charvec_clone(charvec_t *source) {
    charvec_t *temp = charvec_new();
    for (int i = 0; i < source->length; i++) {
        charvec_push(temp, source->data[i]);
    }
    return temp;
}

int charvec_to_int(charvec_t *cv) {
    char *end = &cv->data[cv->length-1];
    return strtol(cv->data, &end, 10);
}

typedef enum {
    CONCAT,
    REPEAT
} opkind_t;

typedef struct op_t {
    struct node_t *left;
    struct node_t *right;
    opkind_t op;
} op_t;

typedef enum {
    BINOP,
    LIT,
    NEOF,
} node_type_t;

typedef struct node_t {
    node_type_t tag;
    union {
        op_t op;
        charvec_t *val;
    };
} node_t;

node_t *node_new() {
    node_t *new = malloc(sizeof(node_t));
    if (new == NULL) abort();
    return new;
}

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

char *token_type_to_str(token_type_t tok) {
    switch (tok) {
        case STRING:
            return "STRING";
        case DOT:
            return "DOT";
        case LPAREN:
            return "LPAREN";
        case RPAREN:
            return "RPAREN";
        case CARET:
            return "CARET";
        case TEOF:
            return "TEOF";
        default:
            abort();
    }
}

token_t lex() {
    token_t temp = {0, 0};

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
                exit(1);
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
        case LPAREN:
        case RPAREN:
        case CARET:
        case TEOF:
            printf("{%s}", token_type_to_str(tok->type));
            break;
        default:
            abort();
            break;
    }
}

typedef struct {
    token_t lookahead;
} parser_t;

token_t parse_next(parser_t *parser) {
    token_t old = parser->lookahead;
    parser->lookahead = lex();
    return old;
}

token_t parse_expect(parser_t *parser, token_type_t type) {
    if (parser->lookahead.type != type) {
        fprintf(stderr, "Expected %s, found %s\n",
                token_type_to_str(type),
                token_type_to_str(parser->lookahead.type));
        exit(1);
    }
    return parse_next(parser);
}

node_t *node_eof() {
    node_t *new = node_new();
    new->tag = NEOF;
    return new;
}

void node_free(node_t *node) {
    switch (node->tag) {
        case LIT:
            charvec_free(node->val);
            break;
        case BINOP:
            node_free(node->op.left);
            node_free(node->op.right);
            break;
        case NEOF:
            break;
        default:
            abort();
     }
    free(node);
}

node_t *parse_E(parser_t *);
node_t *parse_Eprime(parser_t *, node_t *);
node_t *parse_T(parser_t *);
node_t *parse_V(parser_t *);

node_t *parse() {
    parser_t *parser = malloc(sizeof(parser_t));
    if (parser == NULL) abort();
    parser->lookahead = lex();
    node_t *res = parse_E(parser);
    token_free(&parser->lookahead);
    free(parser);
    return res;
}

node_t *parse_E(parser_t *parser) {
    node_t *first = parse_T(parser);
    return parse_Eprime(parser, first);
}

node_t *parse_Eprime(parser_t *parser, node_t *first) {
    if (parser->lookahead.type != DOT) {
        return first;
    }
    parse_expect(parser, DOT);
    node_t *second = parse_T(parser);

    node_t *new = node_new();
    new->tag = BINOP;
    new->op.op = CONCAT;
    new->op.left = first;
    new->op.right = second;

    return parse_Eprime(parser, new);
}

node_t *parse_T(parser_t *parser) {
    node_t *first = parse_V(parser);
    if (parser->lookahead.type != CARET) {
        return first;
    }
    parse_expect(parser, CARET);
    node_t *second = parse_T(parser);

    node_t *new = node_new();
    new->tag = BINOP;
    new->op.op = REPEAT;
    new->op.left = first;
    new->op.right = second;

    return new;
}

node_t *parse_V(parser_t *parser) {
    if (parser->lookahead.type == LPAREN) {
        node_t *inner = parse_E(parser);
        parse_expect(parser, RPAREN);
        return inner;
    }

    if (parser->lookahead.type == TEOF) {
        return node_eof();
    } else {
        token_t tok = parse_expect(parser, STRING);

        node_t *new = node_new();
        new->tag = LIT;
        new->val = tok.val;
        return new;
    }
}

void indent(int depth) {
    for (int i = 0; i < depth; i++) {
        putchar(' ');
    }
}

char *opkind_to_str(opkind_t op) {
    switch (op) {
        case CONCAT:
            return "CONCAT";
        case REPEAT:
            return "REPEAT";
        default:
            abort();
    }
}

void node_print(node_t *node, int depth) {
    switch (node->tag) {
        case BINOP:
            indent(depth);
            printf("BINOP: %s\n", opkind_to_str(node->op.op));
            node_print(node->op.left, depth + 4);
            node_print(node->op.right, depth + 4);
            break;
        case LIT:
            indent(depth);
            printf("STR: %s\n", node->val->data);
            break;
        case NEOF:
            indent(depth);
            printf("EOF\n");
            break;
        default:
            abort();
    }
}

charvec_t *eval(node_t *ast) {
    charvec_t *left, *right, *orig;
    switch (ast->tag) {
        case LIT:
            return charvec_clone(ast->val);
        case BINOP:
            switch (ast->op.op) {
                case CONCAT:
                    left = eval(ast->op.left);
                    right = eval(ast->op.right);
                    charvec_concat(right, left);
                    charvec_free(right);
                    return left;
                case REPEAT:
                    left = eval(ast->op.left);
                    orig = charvec_clone(left);
                    right = eval(ast->op.right);
                    for (int i = 0; i < charvec_to_int(right) - 1; i++) {
                        charvec_concat(orig, left);
                    }
                    charvec_free(orig);
                    charvec_free(right);
                    return left;
                default:
                    abort();
            }
        case NEOF:
            fprintf(stderr, "Error: unexpected EOF...\n");
            node_free(ast);
            exit(1);
        default:
            abort();
    }
}

int main() {
    while (true) {
        node_t *ast = parse();
        node_print(ast, 0);
        charvec_t *res = eval(ast);
        printf("Result: %s\n", res->data);
        node_free(ast);
        charvec_free(res);
    }
}
