/* string_calc: a simple "calculator" on the language of strings.
 *
 * The syntax of the language is:
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
    struct node_t *left;
    struct node_t *right;
    opkind_t op;
} op_t;

typedef enum {
    BINOP,
    LIT
} node_type_t;

typedef struct node_t {
    node_type_t tag;
    union {
        op_t op;
        char *val;
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

typedef struct {
    token_t lookahead;
} parser_t;

token_t parse_next(parser_t *parser) {
    token_t old = parser->lookahead;
    parser->lookahead = lex();
    return old;
}

void parse_expect(parser_t *parser, token_type_t type) {
    if (parser->lookahead.type != type) {
        fprintf(stderr, "Expected %d, found %d\n", type, parser->lookahead.type);
        exit(1);
    }
    parse_next(parser);
}


node_t *parse_E(parser_t *);
node_t *parse_Eprime(parser_t *, node_t *);
node_t *parse_T(parser_t *);
node_t *parse_V(parser_t *);

node_t *parse() {
    parser_t *parser = malloc(sizeof(parser_t));
    if (parser == NULL) abort();
    parse_next(parser);
    return parse_E(parser);
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

    return  parse_Eprime(parser, new);
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

    if (parser->lookahead.type != STRING) {
        fprintf(stderr, "Error: expected STRING, found %d",
                parser->lookahead.type);
        exit(1);
    } else {
        node_t *new = node_new();
        new->tag = LIT;
        new->val = parser->lookahead.val->data;
        parse_next(parser);
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
            printf("STR: %s\n", node->val);
            break;
        default:
            abort();
    }
}

int main(int argc, char **argv) {
    node_t *ast = parse();
    node_print(ast, 0);
}
