/*
 * tinyc - セルフホスト可能な最小限コンパイラ
 *
 * 言語仕様:
 *   - 整数型 (int = 64bit)、ポインタ型 (*T)
 *   - 構造体 (struct)
 *   - 関数定義・呼び出し
 *   - 変数宣言 (var x: int = expr)
 *   - if / while / return
 *   - 文字列リテラル
 *   - 単項: !, -, &, *, sizeof
 *   - 二項: + - * / % == != < <= > >= && || = (代入)
 *   - 添字: a[i]、メンバアクセス: s.f、ポインタメンバ: s->f
 *
 * 出力: x86-64 Linux アセンブリ (AT&T記法, 自作アセンブラ向け)
 * System V AMD64 ABI 準拠
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>

/* ================================================================
   ユーティリティ
   ================================================================ */

static void die(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(1);
}

static void *xmalloc(size_t n) {
    void *p = malloc(n);
    if (!p) die("out of memory");
    return p;
}

static void *xrealloc(void *p, size_t n) {
    p = realloc(p, n);
    if (!p) die("out of memory");
    return p;
}

static char *xstrdup(const char *s) {
    char *p = strdup(s);
    if (!p) die("out of memory");
    return p;
}

static char *xstrndup(const char *s, int n) {
    char *p = xmalloc(n + 1);
    memcpy(p, s, n);
    p[n] = '\0';
    return p;
}

/* 動的配列 */
typedef struct Vec {
    void **data;
    int len, cap;
} Vec;

static Vec *new_vec(void) {
    Vec *v = xmalloc(sizeof(Vec));
    v->data = xmalloc(sizeof(void*) * 8);
    v->len = 0;
    v->cap = 8;
    return v;
}

static void vec_push(Vec *v, void *item) {
    if (v->len == v->cap) {
        v->cap *= 2;
        v->data = xrealloc(v->data, sizeof(void*) * v->cap);
    }
    v->data[v->len++] = item;
}

/* ================================================================
   字句解析 (Lexer)
   ================================================================ */

typedef enum {
    TK_NUM, TK_STR, TK_IDENT,
    TK_PLUS, TK_MINUS, TK_STAR, TK_SLASH, TK_PERCENT,
    TK_AMP, TK_BANG,
    TK_EQ, TK_NEQ, TK_LT, TK_LE, TK_GT, TK_GE,
    TK_AND, TK_OR,
    TK_ASSIGN,
    TK_LPAREN, TK_RPAREN, TK_LBRACE, TK_RBRACE, TK_LBRACKET, TK_RBRACKET,
    TK_COMMA, TK_COLON, TK_SEMICOLON, TK_DOT, TK_ARROW,
    /* キーワード */
    TK_FUNC, TK_VAR, TK_RETURN, TK_IF, TK_ELSE, TK_WHILE,
    TK_STRUCT, TK_SIZEOF,
    TK_EOF,
} TokenKind;

typedef struct Token {
    TokenKind kind;
    long long ival;   /* TK_NUM */
    char *sval;       /* TK_STR, TK_IDENT */
    int line;
} Token;

static Token *new_token(TokenKind kind, int line) {
    Token *t = xmalloc(sizeof(Token));
    t->kind = kind;
    t->ival = 0;
    t->sval = NULL;
    t->line = line;
    return t;
}

typedef struct {
    const char *src;
    int pos;
    int line;
    Vec *tokens;
} Lexer;

static int lex_peek(Lexer *l) {
    return l->src[l->pos];
}
static int lex_next(Lexer *l) {
    int c = l->src[l->pos++];
    if (c == '\n') l->line++;
    return c;
}
static int lex_match(Lexer *l, char c) {
    if (l->src[l->pos] == c) { l->pos++; return 1; }
    return 0;
}

static struct { const char *kw; TokenKind kind; } keywords[] = {
    {"func",   TK_FUNC},
    {"var",    TK_VAR},
    {"return", TK_RETURN},
    {"if",     TK_IF},
    {"else",   TK_ELSE},
    {"while",  TK_WHILE},
    {"struct", TK_STRUCT},
    {"sizeof", TK_SIZEOF},
    {NULL, 0}
};

static Vec *tokenize(const char *src) {
    Lexer l;
    l.src = src;
    l.pos = 0;
    l.line = 1;
    l.tokens = new_vec();

    while (1) {
        /* スキップ: 空白・コメント */
        while (1) {
            while (isspace(lex_peek(&l))) lex_next(&l);
            if (lex_peek(&l) == '/' && l.src[l.pos+1] == '/') {
                while (lex_peek(&l) && lex_peek(&l) != '\n') lex_next(&l);
                continue;
            }
            if (lex_peek(&l) == '/' && l.src[l.pos+1] == '*') {
                l.pos += 2;
                while (lex_peek(&l)) {
                    if (lex_peek(&l) == '*' && l.src[l.pos+1] == '/') {
                        l.pos += 2; break;
                    }
                    lex_next(&l);
                }
                continue;
            }
            break;
        }

        int line = l.line;
        int c = lex_peek(&l);

        if (c == '\0') {
            vec_push(l.tokens, new_token(TK_EOF, line));
            break;
        }

        /* 数値リテラル */
        if (isdigit(c)) {
            Token *t = new_token(TK_NUM, line);
            t->ival = 0;
            if (c == '0' && (l.src[l.pos+1] == 'x' || l.src[l.pos+1] == 'X')) {
                l.pos += 2;
                while (isxdigit(lex_peek(&l))) {
                    int d = lex_next(&l);
                    t->ival = t->ival * 16 + (isdigit(d) ? d-'0' : tolower(d)-'a'+10);
                }
            } else {
                while (isdigit(lex_peek(&l))) t->ival = t->ival * 10 + (lex_next(&l) - '0');
            }
            vec_push(l.tokens, t);
            continue;
        }

        /* 文字列リテラル */
        if (c == '"') {
            lex_next(&l);
            int start = l.pos;
            int len = 0;
            char buf[4096];
            while (lex_peek(&l) != '"' && lex_peek(&l)) {
                int ch = lex_next(&l);
                if (ch == '\\') {
                    ch = lex_next(&l);
                    switch (ch) {
                        case 'n': buf[len++] = '\n'; break;
                        case 't': buf[len++] = '\t'; break;
                        case '0': buf[len++] = '\0'; break;
                        case '"': buf[len++] = '"'; break;
                        case '\\': buf[len++] = '\\'; break;
                        default: buf[len++] = ch; break;
                    }
                } else {
                    buf[len++] = ch;
                }
                (void)start;
            }
            buf[len] = '\0';
            if (lex_peek(&l) != '"') die("line %d: unterminated string", line);
            lex_next(&l);
            Token *t = new_token(TK_STR, line);
            t->sval = xstrndup(buf, len);
            t->ival = len; /* 長さ(NUL除く) */
            vec_push(l.tokens, t);
            continue;
        }

        /* 識別子・キーワード */
        if (isalpha(c) || c == '_') {
            int start = l.pos;
            while (isalnum(lex_peek(&l)) || lex_peek(&l) == '_') lex_next(&l);
            char *ident = xstrndup(l.src + start, l.pos - start);
            TokenKind kind = TK_IDENT;
            for (int i = 0; keywords[i].kw; i++) {
                if (strcmp(ident, keywords[i].kw) == 0) {
                    kind = keywords[i].kind;
                    break;
                }
            }
            Token *t = new_token(kind, line);
            t->sval = ident;
            vec_push(l.tokens, t);
            continue;
        }

        lex_next(&l);
        switch (c) {
#define TOK(ch, k) case ch: vec_push(l.tokens, new_token(k, line)); break
            TOK('(', TK_LPAREN);
            TOK(')', TK_RPAREN);
            TOK('{', TK_LBRACE);
            TOK('}', TK_RBRACE);
            TOK('[', TK_LBRACKET);
            TOK(']', TK_RBRACKET);
            TOK(',', TK_COMMA);
            TOK(':', TK_COLON);
            TOK(';', TK_SEMICOLON);
            TOK('%', TK_PERCENT);
#undef TOK
            case '+': vec_push(l.tokens, new_token(TK_PLUS, line)); break;
            case '-':
                if (lex_match(&l, '>'))
                    vec_push(l.tokens, new_token(TK_ARROW, line));
                else
                    vec_push(l.tokens, new_token(TK_MINUS, line));
                break;
            case '*': vec_push(l.tokens, new_token(TK_STAR, line)); break;
            case '/': vec_push(l.tokens, new_token(TK_SLASH, line)); break;
            case '&':
                if (lex_match(&l, '&')) vec_push(l.tokens, new_token(TK_AND, line));
                else vec_push(l.tokens, new_token(TK_AMP, line));
                break;
            case '|':
                if (lex_match(&l, '|')) vec_push(l.tokens, new_token(TK_OR, line));
                else die("line %d: unexpected '|'", line);
                break;
            case '!':
                if (lex_match(&l, '=')) vec_push(l.tokens, new_token(TK_NEQ, line));
                else vec_push(l.tokens, new_token(TK_BANG, line));
                break;
            case '=':
                if (lex_match(&l, '=')) vec_push(l.tokens, new_token(TK_EQ, line));
                else vec_push(l.tokens, new_token(TK_ASSIGN, line));
                break;
            case '<':
                if (lex_match(&l, '=')) vec_push(l.tokens, new_token(TK_LE, line));
                else vec_push(l.tokens, new_token(TK_LT, line));
                break;
            case '>':
                if (lex_match(&l, '=')) vec_push(l.tokens, new_token(TK_GE, line));
                else vec_push(l.tokens, new_token(TK_GT, line));
                break;
            case '.': vec_push(l.tokens, new_token(TK_DOT, line)); break;
            default:
                die("line %d: unexpected character '%c'", line, c);
        }
    }
    return l.tokens;
}

/* ================================================================
   型システム
   ================================================================ */

typedef enum {
    TY_INT,     /* int (64bit) */
    TY_PTR,     /* *T */
    TY_ARRAY,   /* T[n] */
    TY_STRUCT,  /* struct S */
    TY_FUNC,    /* 関数型 (コード生成用) */
    TY_VOID,    /* void (返り値なし) */
} TypeKind;

typedef struct Type Type;
typedef struct Field Field;

struct Field {
    char *name;
    Type *type;
    int offset;  /* バイトオフセット */
};

struct Type {
    TypeKind kind;
    Type *base;       /* PTR/ARRAY の要素型 */
    int array_len;    /* ARRAY */
    char *name;       /* STRUCT の名前 */
    Vec *fields;      /* STRUCT のフィールド */
    int size;         /* バイトサイズ (計算済み) */
    Vec *params;      /* FUNC のパラメータ型 */
    Type *ret;        /* FUNC の返り値型 */
};

static Type *ty_int;
static Type *ty_byte;
static Type *ty_void;

static Type *new_type(TypeKind kind) {
    Type *t = xmalloc(sizeof(Type));
    t->kind = kind;
    t->base = NULL;
    t->array_len = 0;
    t->name = NULL;
    t->fields = NULL;
    t->size = 0;
    t->params = NULL;
    t->ret = NULL;
    return t;
}

static Type *ptr_to(Type *base) {
    Type *t = new_type(TY_PTR);
    t->base = base;
    t->size = 8;
    return t;
}

static Type *array_of(Type *base, int len) {
    Type *t = new_type(TY_ARRAY);
    t->base = base;
    t->array_len = len;
    t->size = base->size * len;
    return t;
}

static int type_size(Type *t) {
    return t->size;
}

static int is_ptr(Type *t) {
    return t->kind == TY_PTR || t->kind == TY_ARRAY;
}

static Type *deref_type(Type *t) {
    if (t->kind == TY_PTR) return t->base;
    if (t->kind == TY_ARRAY) return t->base;
    die("not a pointer type");
    return NULL;
}

/* ================================================================
   AST ノード
   ================================================================ */

typedef enum {
    /* 式 */
    ND_NUM,       /* 整数定数 */
    ND_STR,       /* 文字列リテラル */
    ND_VAR,       /* 変数参照 */
    ND_ADDR,      /* &expr */
    ND_DEREF,     /* *expr */
    ND_NEG,       /* -expr */
    ND_NOT,       /* !expr */
    ND_SIZEOF,    /* sizeof(type) */
    ND_ADD, ND_SUB, ND_MUL, ND_DIV, ND_MOD,
    ND_EQ, ND_NEQ, ND_LT, ND_LE,
    ND_AND, ND_OR,
    ND_ASSIGN,    /* lhs = rhs */
    ND_INDEX,     /* arr[idx] */
    ND_MEMBER,    /* expr.field */
    ND_CALL,      /* func(args) */
    /* 文 */
    ND_BLOCK,     /* { stmts } */
    ND_VAR_DECL,  /* var x: T = init */
    ND_RETURN,
    ND_IF,
    ND_WHILE,
    ND_EXPR_STMT, /* 式文 */
    /* トップレベル */
    ND_FUNC,
    ND_STRUCT_DEF,
    ND_EXTERN_DECL,  /* 外部関数宣言 (body なし) */
    ND_GLOBAL_VAR,   /* グローバル変数宣言 */
} NodeKind;

typedef struct Node Node;
typedef struct Var Var;

struct Var {
    char *name;
    Type *type;
    int offset;     /* ローカル: rbpからのオフセット (負) */
    int is_global;
    char *label;    /* グローバル変数のラベル */
};

struct Node {
    NodeKind kind;
    Type *type;
    int line;

    /* 数値定数 */
    long long ival;
    /* 文字列 */
    char *sval;
    int slen;
    int str_label;  /* .LC<n> */

    /* 変数 */
    Var *var;

    /* 単項・二項 */
    Node *lhs, *rhs;

    /* sizeof */
    Type *sizeof_type;

    /* メンバアクセス */
    char *field_name;
    Field *field;  /* 解決済み */

    /* 関数呼び出し */
    char *func_name;
    Vec *args;

    /* ブロック・if/while */
    Vec *stmts;
    Node *cond;
    Node *then_;
    Node *else_;
    Node *body;

    /* 変数宣言 */
    Node *init;  /* 初期化式 (NULL可) */

    /* 関数定義 */
    char *name;
    Vec *params;  /* Var* */
    Type *ret_type;
    Node *func_body;
    int local_size; /* コード生成時に確定 */
};

static Node *new_node(NodeKind kind, int line) {
    Node *n = xmalloc(sizeof(Node));
    memset(n, 0, sizeof(Node));
    n->kind = kind;
    n->line = line;
    return n;
}

/* ================================================================
   パーサー
   ================================================================ */

typedef struct {
    Vec *tokens;
    int pos;
    /* 構造体テーブル */
    Vec *structs;   /* Type* (TY_STRUCT) */
} Parser;

static Token *peek(Parser *p) {
    return p->tokens->data[p->pos];
}

static Token *consume(Parser *p) {
    return p->tokens->data[p->pos++];
}

static Token *expect(Parser *p, TokenKind kind) {
    Token *t = consume(p);
    if (t->kind != kind) die("line %d: unexpected token", t->line);
    return t;
}

static int check(Parser *p, TokenKind kind) {
    return peek(p)->kind == kind;
}

static int match(Parser *p, TokenKind kind) {
    if (check(p, kind)) { consume(p); return 1; }
    return 0;
}

/* 型パース */
static Type *parse_type(Parser *p);

static Type *parse_base_type(Parser *p) {
    Token *t = consume(p);
    if (t->kind == TK_IDENT && strcmp(t->sval, "int") == 0) return ty_int;
    if (t->kind == TK_IDENT && strcmp(t->sval, "byte") == 0) return ty_byte;
    if (t->kind == TK_IDENT && strcmp(t->sval, "void") == 0) return ty_void;
    /* "struct Foo" または単に "Foo" (struct名) */
    if (t->kind == TK_STRUCT || t->kind == TK_IDENT) {
        const char *sname;
        if (t->kind == TK_STRUCT) {
            Token *name2 = expect(p, TK_IDENT);
            sname = name2->sval;
        } else {
            sname = t->sval;
        }
        for (int i = 0; i < p->structs->len; i++) {
            Type *s = p->structs->data[i];
            if (strcmp(s->name, sname) == 0) return s;
        }
        /* 前方宣言として空の構造体型を返す */
        Type *s = new_type(TY_STRUCT);
        s->name = xstrdup(sname);
        vec_push(p->structs, s);
        return s;
    }
    die("line %d: expected type, got '%s'", t->line, t->sval ? t->sval : "?");
    return NULL;
}

static Type *parse_type(Parser *p) {
    /* ポインタ */
    if (match(p, TK_STAR)) {
        Type *base = parse_type(p);
        return ptr_to(base);
    }
    Type *base = parse_base_type(p);
    /* 配列 */
    while (check(p, TK_LBRACKET)) {
        consume(p);
        Token *n = expect(p, TK_NUM);
        expect(p, TK_RBRACKET);
        base = array_of(base, (int)n->ival);
    }
    return base;
}

/* 前方宣言 */
static Node *parse_expr(Parser *p);
static Node *parse_stmt(Parser *p);
static Node *parse_block(Parser *p);

/* 変数スコープ */
typedef struct Scope Scope;
struct Scope {
    Vec *vars;   /* Var* */
    Scope *parent;
};

static Scope *current_scope;
static int current_offset; /* 現在のローカル変数オフセット */
static Vec *all_locals;    /* 関数内の全Var* */

static Scope *push_scope(void) {
    Scope *s = xmalloc(sizeof(Scope));
    s->vars = new_vec();
    s->parent = current_scope;
    current_scope = s;
    return s;
}

static void pop_scope(void) {
    current_scope = current_scope->parent;
}

static Var *find_var(const char *name) {
    for (Scope *s = current_scope; s; s = s->parent) {
        for (int i = 0; i < s->vars->len; i++) {
            Var *v = s->vars->data[i];
            if (strcmp(v->name, name) == 0) return v;
        }
    }
    return NULL;
}

static Var *new_local(const char *name, Type *type) {
    Var *v = xmalloc(sizeof(Var));
    v->name = xstrdup(name);
    v->type = type;
    /* アライメント */
    int sz = type_size(type);
    if (sz < 8) sz = 8;
    current_offset -= sz;
    v->offset = current_offset;
    v->is_global = 0;
    v->label = NULL;
    vec_push(current_scope->vars, v);
    vec_push(all_locals, v);
    return v;
}

/* 関数テーブル (シグネチャチェック用・省略: 名前のみ) */
static Vec *func_names;
static Vec *func_ret_types; /* 関数名に対応する返り値型 */


/* 式パース */

static Node *parse_primary(Parser *p) {
    Token *t = peek(p);

    if (t->kind == TK_NUM) {
        consume(p);
        Node *n = new_node(ND_NUM, t->line);
        n->ival = t->ival;
        n->type = ty_int;
        return n;
    }

    if (t->kind == TK_STR) {
        consume(p);
        static int str_count = 0;
        Node *n = new_node(ND_STR, t->line);
        n->sval = t->sval;
        n->slen = (int)t->ival;
        n->str_label = str_count++;
        n->type = ptr_to(ty_byte); /* char* */
        return n;
    }

    if (t->kind == TK_IDENT) {
        consume(p);
        /* 関数呼び出し */
        if (check(p, TK_LPAREN)) {
            consume(p);
            Node *n = new_node(ND_CALL, t->line);
            n->func_name = t->sval;
            n->args = new_vec();
            /* 返り値型を関数テーブルから検索 */
            n->type = ty_int; /* デフォルト */
            for (int fi = 0; fi < func_names->len; fi++) {
                if (strcmp((char*)func_names->data[fi], t->sval) == 0) {
                    Type *rt = (Type*)func_ret_types->data[fi];
                    if (rt) n->type = rt;
                    break;
                }
            }
            if (!check(p, TK_RPAREN)) {
                vec_push(n->args, parse_expr(p));
                while (match(p, TK_COMMA))
                    vec_push(n->args, parse_expr(p));
            }
            expect(p, TK_RPAREN);
            return n;
        }
        /* 変数 */
        Var *v = find_var(t->sval);
        if (!v) die("line %d: undefined variable '%s'", t->line, t->sval);
        Node *n = new_node(ND_VAR, t->line);
        n->var = v;
        n->type = v->type;
        return n;
    }

    if (t->kind == TK_SIZEOF) {
        /* sizeof already peeked as t, now consume it */
        consume(p); /* sizeof */
        expect(p, TK_LPAREN);
        Type *ty;
        /* sizeof(型) か sizeof(式) */
        /* 簡略: 識別子が "int" や "struct" なら型、それ以外は式 */
        Token *next = peek(p);
        if (next->kind == TK_STAR ||
            (next->kind == TK_IDENT && (strcmp(next->sval,"int")==0||strcmp(next->sval,"void")==0)) ||
            next->kind == TK_STRUCT) {
            ty = parse_type(p);
        } else {
            Node *e = parse_expr(p);
            ty = e->type;
        }
        expect(p, TK_RPAREN);
        Node *n = new_node(ND_SIZEOF, t->line);
        n->sizeof_type = ty;
        n->type = ty_int;
        n->ival = type_size(ty);
        return n;
    }

    if (t->kind == TK_LPAREN) {
        consume(p);
        Node *n = parse_expr(p);
        expect(p, TK_RPAREN);
        return n;
    }

    die("line %d: unexpected token in expression", t->line);
    return NULL;
}

/* sizeof の consume が間違いなので修正 */
/* (上のコードを後でパッチ) */

static Node *parse_postfix(Parser *p) {
    Node *n = parse_primary(p);
    while (1) {
        if (check(p, TK_LBRACKET)) {
            int line = peek(p)->line;
            consume(p);
            Node *idx = parse_expr(p);
            expect(p, TK_RBRACKET);
            Node *nd = new_node(ND_INDEX, line);
            nd->lhs = n;
            nd->rhs = idx;
            /* 型: 配列/ポインタの要素型 */
            if (n->type->kind == TY_PTR || n->type->kind == TY_ARRAY)
                nd->type = n->type->base;
            else
                die("line %d: subscript of non-array", line);
            n = nd;
            continue;
        }
        if (check(p, TK_DOT) || check(p, TK_ARROW)) {
            int is_arrow = peek(p)->kind == TK_ARROW;
            int line = peek(p)->line;
            consume(p);
            Token *fname = expect(p, TK_IDENT);
            Node *nd = new_node(ND_MEMBER, line);
            nd->lhs = n;
            nd->field_name = fname->sval;
            if (is_arrow) {
                /* (*n).field に変換 */
                Node *deref = new_node(ND_DEREF, line);
                deref->lhs = n;
                if (n->type->kind == TY_PTR)
                    deref->type = n->type->base;
                else
                    die("line %d: -> on non-pointer", line);
                nd->lhs = deref;
            }
            /* フィールドの型を解決 */
            {
                Type *sty = nd->lhs->type;
                nd->type = ty_int; /* デフォルト */
                if (sty->kind == TY_STRUCT && sty->fields) {
                    for (int fi = 0; fi < sty->fields->len; fi++) {
                        Field *ff = sty->fields->data[fi];
                        if (strcmp(ff->name, fname->sval) == 0) {
                            nd->type = ff->type;
                            break;
                        }
                    }
                }
            }
            n = nd;
            continue;
        }
        break;
    }
    return n;
}

static Node *parse_unary(Parser *p) {
    Token *t = peek(p);
    if (t->kind == TK_MINUS) {
        consume(p);
        Node *n = new_node(ND_NEG, t->line);
        n->lhs = parse_unary(p);
        n->type = ty_int;
        return n;
    }
    if (t->kind == TK_BANG) {
        consume(p);
        Node *n = new_node(ND_NOT, t->line);
        n->lhs = parse_unary(p);
        n->type = ty_int;
        return n;
    }
    if (t->kind == TK_AMP) {
        consume(p);
        Node *n = new_node(ND_ADDR, t->line);
        n->lhs = parse_unary(p);
        n->type = ptr_to(n->lhs->type);
        return n;
    }
    if (t->kind == TK_STAR) {
        consume(p);
        Node *n = new_node(ND_DEREF, t->line);
        n->lhs = parse_unary(p);
        if (!is_ptr(n->lhs->type))
            die("line %d: dereference of non-pointer", t->line);
        n->type = deref_type(n->lhs->type);
        return n;
    }
    if (t->kind == TK_SIZEOF) {
        consume(p);
        int line = t->line;
        expect(p, TK_LPAREN);
        Token *next = peek(p);
        Type *ty;
        if (next->kind == TK_STAR ||
            (next->kind == TK_IDENT && (strcmp(next->sval,"int")==0||strcmp(next->sval,"void")==0)) ||
            next->kind == TK_STRUCT) {
            ty = parse_type(p);
        } else {
            Node *e = parse_expr(p);
            ty = e->type;
        }
        expect(p, TK_RPAREN);
        Node *n = new_node(ND_NUM, line);
        n->type = ty_int;
        n->ival = type_size(ty);
        return n;
    }
    return parse_postfix(p);
}

static Node *parse_mul(Parser *p) {
    Node *n = parse_unary(p);
    while (check(p, TK_STAR) || check(p, TK_SLASH) || check(p, TK_PERCENT)) {
        Token *op = consume(p);
        NodeKind k = op->kind == TK_STAR ? ND_MUL :
                     op->kind == TK_SLASH ? ND_DIV : ND_MOD;
        Node *nd = new_node(k, op->line);
        nd->lhs = n;
        nd->rhs = parse_unary(p);
        nd->type = ty_int;
        n = nd;
    }
    return n;
}

static Node *parse_add(Parser *p) {
    Node *n = parse_mul(p);
    while (check(p, TK_PLUS) || check(p, TK_MINUS)) {
        Token *op = consume(p);
        NodeKind k = op->kind == TK_PLUS ? ND_ADD : ND_SUB;
        Node *nd = new_node(k, op->line);
        nd->lhs = n;
        nd->rhs = parse_mul(p);
        /* ポインタ算術 */
        if (is_ptr(n->type)) {
            nd->type = n->type;
        } else if (is_ptr(nd->rhs->type)) {
            nd->type = nd->rhs->type;
        } else {
            nd->type = ty_int;
        }
        n = nd;
    }
    return n;
}

static Node *parse_rel(Parser *p) {
    Node *n = parse_add(p);
    while (check(p, TK_LT) || check(p, TK_LE) || check(p, TK_GT) || check(p, TK_GE)) {
        Token *op = consume(p);
        NodeKind k;
        Node *lhs = n, *rhs = parse_add(p);
        switch (op->kind) {
            case TK_LT: k = ND_LT; break;
            case TK_LE: k = ND_LE; break;
            case TK_GT: k = ND_LT; { Node *tmp = lhs; lhs = rhs; rhs = tmp; } break;
            case TK_GE: k = ND_LE; { Node *tmp = lhs; lhs = rhs; rhs = tmp; } break;
            default: k = ND_LT; break;
        }
        Node *nd = new_node(k, op->line);
        nd->lhs = lhs; nd->rhs = rhs;
        nd->type = ty_int;
        n = nd;
    }
    return n;
}

static Node *parse_eq(Parser *p) {
    Node *n = parse_rel(p);
    while (check(p, TK_EQ) || check(p, TK_NEQ)) {
        Token *op = consume(p);
        Node *nd = new_node(op->kind == TK_EQ ? ND_EQ : ND_NEQ, op->line);
        nd->lhs = n;
        nd->rhs = parse_rel(p);
        nd->type = ty_int;
        n = nd;
    }
    return n;
}

static Node *parse_logand(Parser *p) {
    Node *n = parse_eq(p);
    while (check(p, TK_AND)) {
        Token *op = consume(p);
        Node *nd = new_node(ND_AND, op->line);
        nd->lhs = n; nd->rhs = parse_eq(p);
        nd->type = ty_int;
        n = nd;
    }
    return n;
}

static Node *parse_logor(Parser *p) {
    Node *n = parse_logand(p);
    while (check(p, TK_OR)) {
        Token *op = consume(p);
        Node *nd = new_node(ND_OR, op->line);
        nd->lhs = n; nd->rhs = parse_logand(p);
        nd->type = ty_int;
        n = nd;
    }
    return n;
}

static Node *parse_assign(Parser *p) {
    Node *n = parse_logor(p);
    if (check(p, TK_ASSIGN)) {
        Token *op = consume(p);
        Node *nd = new_node(ND_ASSIGN, op->line);
        nd->lhs = n;
        nd->rhs = parse_assign(p);
        nd->type = n->type;
        return nd;
    }
    return n;
}

static Node *parse_expr(Parser *p) {
    return parse_assign(p);
}

static Node *parse_stmt(Parser *p) {
    Token *t = peek(p);

    if (t->kind == TK_RETURN) {
        consume(p);
        Node *n = new_node(ND_RETURN, t->line);
        if (!check(p, TK_SEMICOLON))
            n->lhs = parse_expr(p);
        expect(p, TK_SEMICOLON);
        return n;
    }

    if (t->kind == TK_IF) {
        consume(p);
        Node *n = new_node(ND_IF, t->line);
        n->cond = parse_expr(p);
        n->then_ = parse_block(p);
        if (match(p, TK_ELSE))
            n->else_ = check(p, TK_IF) ? parse_stmt(p) : parse_block(p);
        return n;
    }

    if (t->kind == TK_WHILE) {
        consume(p);
        Node *n = new_node(ND_WHILE, t->line);
        n->cond = parse_expr(p);
        n->body = parse_block(p);
        return n;
    }

    if (t->kind == TK_LBRACE) {
        return parse_block(p);
    }

    if (t->kind == TK_VAR) {
        consume(p);
        Token *name = expect(p, TK_IDENT);
        expect(p, TK_COLON);
        Type *ty = parse_type(p);
        Var *v = new_local(name->sval, ty);
        Node *n = new_node(ND_VAR_DECL, t->line);
        n->var = v;
        if (match(p, TK_ASSIGN))
            n->init = parse_expr(p);
        expect(p, TK_SEMICOLON);
        return n;
    }

    /* 式文 */
    Node *n = new_node(ND_EXPR_STMT, t->line);
    n->lhs = parse_expr(p);
    expect(p, TK_SEMICOLON);
    return n;
}

static Node *parse_block(Parser *p) {
    Token *t = expect(p, TK_LBRACE);
    Node *n = new_node(ND_BLOCK, t->line);
    n->stmts = new_vec();
    push_scope();
    while (!check(p, TK_RBRACE) && !check(p, TK_EOF)) {
        vec_push(n->stmts, parse_stmt(p));
    }
    expect(p, TK_RBRACE);
    pop_scope();
    return n;
}

/* 構造体定義 */
static void parse_struct_def(Parser *p) {
    expect(p, TK_STRUCT);
    Token *name = expect(p, TK_IDENT);
    expect(p, TK_LBRACE);

    /* 既存の型を探す (前方宣言済みかも) */
    Type *s = NULL;
    for (int i = 0; i < p->structs->len; i++) {
        Type *t2 = p->structs->data[i];
        if (strcmp(t2->name, name->sval) == 0) { s = t2; break; }
    }
    if (!s) {
        s = new_type(TY_STRUCT);
        s->name = xstrdup(name->sval);
        vec_push(p->structs, s);
    }
    s->fields = new_vec();

    int offset = 0;
    while (!check(p, TK_RBRACE)) {
        Token *fname = expect(p, TK_IDENT);
        expect(p, TK_COLON);
        Type *ftype = parse_type(p);
        expect(p, TK_SEMICOLON);

        /* アライメント (8バイト境界) */
        int sz = type_size(ftype);
        int align = sz < 8 ? sz : 8;
        if (offset % align != 0)
            offset += align - (offset % align);

        Field *f = xmalloc(sizeof(Field));
        f->name = xstrdup(fname->sval);
        f->type = ftype;
        f->offset = offset;
        vec_push(s->fields, f);
        offset += sz;
    }
    /* 全体サイズを8バイト境界にアライン */
    if (offset % 8 != 0) offset += 8 - (offset % 8);
    s->size = offset;
    expect(p, TK_RBRACE);
}

/* 関数定義 */
static Node *parse_func(Parser *p) {
    Token *t = expect(p, TK_FUNC);
    Token *name = expect(p, TK_IDENT);
    vec_push(func_names, name->sval);
    /* ret_typeは後で確定するので仮にNULLを入れる (indexを合わせるため) */
    vec_push(func_ret_types, NULL);

    Node *n = new_node(ND_FUNC, t->line);
    n->name = name->sval;
    n->params = new_vec();

    /* ローカル変数オフセットリセット */
    current_offset = 0;
    all_locals = new_vec();
    push_scope();

    expect(p, TK_LPAREN);
    if (!check(p, TK_RPAREN)) {
        do {
            Token *pname = expect(p, TK_IDENT);
            expect(p, TK_COLON);
            Type *pty = parse_type(p);
            Var *v = new_local(pname->sval, pty);
            vec_push(n->params, v);
        } while (match(p, TK_COMMA));
    }
    expect(p, TK_RPAREN);

    /* 返り値型 */
    if (match(p, TK_COLON))
        n->ret_type = parse_type(p);
    else
        n->ret_type = ty_void;
    /* 返り値型テーブルを更新 */
    func_ret_types->data[func_ret_types->len - 1] = n->ret_type;

    /* 外部宣言: body なしで ';' だけ */
    if (match(p, TK_SEMICOLON)) {
        pop_scope();
        n->kind = ND_EXTERN_DECL;
        return n;
    }

    /* 本体 (スコープは既に open) */
    Token *lb = expect(p, TK_LBRACE);
    Node *body = new_node(ND_BLOCK, lb->line);
    body->stmts = new_vec();
    while (!check(p, TK_RBRACE) && !check(p, TK_EOF)) {
        vec_push(body->stmts, parse_stmt(p));
    }
    expect(p, TK_RBRACE);
    pop_scope();

    n->func_body = body;
    /* local_size は後で確定 */
    n->local_size = -current_offset;

    return n;
}

static Vec *parse_program(const char *src) {
    Vec *tokens = tokenize(src);
    Parser p;
    p.tokens = tokens;
    p.pos = 0;
    p.structs = new_vec();
    func_names = new_vec();
    func_ret_types = new_vec();

    /* 型初期化 */
    ty_int = new_type(TY_INT); ty_int->size = 8;
    ty_byte = new_type(TY_INT); ty_byte->size = 1;
    ty_void = new_type(TY_VOID); ty_void->size = 0;

    Vec *nodes = new_vec();
    /* 一度全関数名を先読み (前方参照対応は省略: 宣言順依存) */

    while (!check(&p, TK_EOF)) {
        if (check(&p, TK_STRUCT)) {
            parse_struct_def(&p);
        } else if (check(&p, TK_FUNC)) {
            vec_push(nodes, parse_func(&p));
        } else if (check(&p, TK_VAR)) {
            /* グローバル変数宣言: var name: Type = expr; */
            Token *vt = consume(&p);
            Token *vname = expect(&p, TK_IDENT);
            expect(&p, TK_COLON);
            Type *vtype = parse_type(&p);
            Node *gv = new_node(ND_GLOBAL_VAR, vt->line);
            /* ラベル名 = 変数名 */
            Var *v = xmalloc(sizeof(Var));
            v->name = vname->sval;
            v->type = vtype;
            v->is_global = 1;
            v->label = vname->sval;
            v->offset = 0;
            gv->var = v;
            /* 初期値 (整数定数のみサポート) */
            gv->ival = 0;
            if (match(&p, TK_ASSIGN)) {
                Token *init = expect(&p, TK_NUM);
                gv->ival = init->ival;
            }
            expect(&p, TK_SEMICOLON);
            /* グローバルスコープに登録 */
            if (!current_scope) push_scope();
            vec_push(current_scope->vars, v);
            vec_push(nodes, gv);
        } else {
            die("line %d: expected func or struct", peek(&p)->line);
        }
    }

    /* 構造体メンバ型解決は既にパース中に行われている */
    /* (前方参照があれば size=0 のまま → エラー) */

    return nodes;
}

/* ================================================================
   コード生成 (x86-64 Linux, AT&T記法)
   ================================================================ */

static FILE *out;
static int label_count = 0;

static int new_label(void) {
    return label_count++;
}

static void emit(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(out, fmt, ap);
    fprintf(out, "\n");
    va_end(ap);
}

/* 文字列リテラルの収集 */
typedef struct { int label; char *data; int len; } StrLiteral;
static Vec *str_literals;

/* 関数呼び出し規約: 引数レジスタ */
static const char *argregs[] = { "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
#define MAX_ARGS 6

/* アドレスをスタックにpush */
static void gen_addr(Node *n);
/* 値をスタックにpush */
static void gen_val(Node *n);
/* 値をスタックからpopして代入 */

/* スタック深度追跡 (省略: 単純実装) */
static int stack_depth = 0;

static void push_reg(const char *reg) {
    emit("  pushq %s", reg);
    stack_depth++;
}

static void pop_reg(const char *reg) {
    emit("  popq %s", reg);
    stack_depth--;
}

/* メモリロード (サイズに応じて) */
static void load(Type *ty) {
    pop_reg("%rax"); /* アドレス */
    if (ty->kind == TY_ARRAY) {
        /* 配列はアドレスそのものを値として使う */
        push_reg("%rax");
        return;
    }
    int sz = type_size(ty);
    if (sz == 1)      emit("  movsbq (%%rax), %%rax");
    else if (sz == 2) emit("  movswq (%%rax), %%rax");
    else if (sz == 4) emit("  movslq (%%rax), %%rax");
    else              emit("  movq (%%rax), %%rax");
    push_reg("%rax");
}

/* メモリストア */
static void store(Type *ty) {
    pop_reg("%rdi"); /* 値 */
    pop_reg("%rax"); /* アドレス */
    int sz = type_size(ty);
    if (sz == 1)      emit("  movb %%dil, (%%rax)");
    else if (sz == 2) emit("  movw %%di, (%%rax)");
    else if (sz == 4) emit("  movl %%edi, (%%rax)");
    else              emit("  movq %%rdi, (%%rax)");
    push_reg("%rdi"); /* 値を残す */
}

static void gen_addr(Node *n) {
    switch (n->kind) {
        case ND_VAR:
            if (n->var->is_global)
                emit("  leaq %s(%%rip), %%rax", n->var->label);
            else
                emit("  leaq %d(%%rbp), %%rax", n->var->offset);
            push_reg("%rax");
            return;
        case ND_DEREF:
            gen_val(n->lhs);
            return;
        case ND_INDEX: {
            /* arr[i] → addr(arr) + i * elem_size */
            gen_addr(n->lhs);
            if (n->lhs->type->kind == TY_ARRAY) {
                /* addrは既にスタックにある */
            } else {
                /* ポインタ → ロード */
                load(n->lhs->type);
            }
            gen_val(n->rhs);
            pop_reg("%rcx"); /* index */
            pop_reg("%rax"); /* base addr */
            int esz = type_size(n->type);
            emit("  imulq $%d, %%rcx", esz);
            emit("  addq %%rcx, %%rax");
            push_reg("%rax");
            return;
        }
        case ND_MEMBER: {
            /* 構造体のアドレス + field offset */
            Type *sty = n->lhs->type;
            if (sty->kind != TY_STRUCT)
                die("line %d: member access on non-struct", n->line);
            /* フィールドを検索 */
            Field *f = NULL;
            for (int i = 0; i < sty->fields->len; i++) {
                Field *fi = sty->fields->data[i];
                if (strcmp(fi->name, n->field_name) == 0) { f = fi; break; }
            }
            if (!f) die("line %d: no field '%s'", n->line, n->field_name);
            n->field = f;
            n->type = f->type;
            gen_addr(n->lhs);
            pop_reg("%rax");
            emit("  addq $%d, %%rax", f->offset);
            push_reg("%rax");
            return;
        }
        default:
            die("line %d: not an lvalue", n->line);
    }
}

static void gen_val(Node *n) {
    switch (n->kind) {
        case ND_NUM:
            emit("  movq $%lld, %%rax", n->ival);
            push_reg("%rax");
            return;
        case ND_STR: {
            /* 文字列リテラルを登録 */
            StrLiteral *sl = xmalloc(sizeof(StrLiteral));
            sl->label = n->str_label;
            sl->data = n->sval;
            sl->len = n->slen;
            vec_push(str_literals, sl);
            emit("  leaq .LC%d(%%rip), %%rax", n->str_label);
            push_reg("%rax");
            return;
        }
        case ND_VAR:
        case ND_INDEX:
        case ND_MEMBER:
            gen_addr(n);
            load(n->type);
            return;
        case ND_DEREF:
            gen_val(n->lhs);
            load(n->type);
            return;
        case ND_ADDR:
            gen_addr(n->lhs);
            return;
        case ND_NEG:
            gen_val(n->lhs);
            pop_reg("%rax");
            emit("  negq %%rax");
            push_reg("%rax");
            return;
        case ND_NOT:
            gen_val(n->lhs);
            pop_reg("%rax");
            emit("  cmpq $0, %%rax");
            emit("  sete %%al");
            emit("  movzbq %%al, %%rax");
            push_reg("%rax");
            return;
        case ND_SIZEOF:
            emit("  movq $%lld, %%rax", n->ival);
            push_reg("%rax");
            return;
        case ND_ASSIGN:
            gen_addr(n->lhs);
            gen_val(n->rhs);
            store(n->lhs->type);
            return;
        case ND_ADD: {
            gen_val(n->lhs);
            gen_val(n->rhs);
            pop_reg("%rdi");
            pop_reg("%rax");
            /* ポインタ算術 */
            if (is_ptr(n->lhs->type)) {
                emit("  imulq $%d, %%rdi", type_size(n->lhs->type->base));
            } else if (is_ptr(n->rhs->type)) {
                emit("  imulq $%d, %%rax", type_size(n->rhs->type->base));
            }
            emit("  addq %%rdi, %%rax");
            push_reg("%rax");
            return;
        }
        case ND_SUB: {
            gen_val(n->lhs);
            gen_val(n->rhs);
            pop_reg("%rdi");
            pop_reg("%rax");
            if (is_ptr(n->lhs->type) && !is_ptr(n->rhs->type)) {
                emit("  imulq $%d, %%rdi", type_size(n->lhs->type->base));
            }
            emit("  subq %%rdi, %%rax");
            /* ポインタ同士の引き算: 要素数 */
            if (is_ptr(n->lhs->type) && is_ptr(n->rhs->type)) {
                emit("  cqo");
                emit("  movq $%d, %%rdi", type_size(n->lhs->type->base));
                emit("  idivq %%rdi");
            }
            push_reg("%rax");
            return;
        }
        case ND_MUL:
            gen_val(n->lhs);
            gen_val(n->rhs);
            pop_reg("%rdi");
            pop_reg("%rax");
            emit("  imulq %%rdi, %%rax");
            push_reg("%rax");
            return;
        case ND_DIV:
        case ND_MOD:
            gen_val(n->lhs);
            gen_val(n->rhs);
            pop_reg("%rcx");
            pop_reg("%rax");
            emit("  cqo");
            emit("  idivq %%rcx");
            if (n->kind == ND_DIV)
                push_reg("%rax");
            else {
                emit("  movq %%rdx, %%rax");
                push_reg("%rax");
            }
            return;
        case ND_EQ: case ND_NEQ: case ND_LT: case ND_LE:
            gen_val(n->lhs);
            gen_val(n->rhs);
            pop_reg("%rdi");
            pop_reg("%rax");
            emit("  cmpq %%rdi, %%rax");
            if (n->kind == ND_EQ)  emit("  sete %%al");
            if (n->kind == ND_NEQ) emit("  setne %%al");
            if (n->kind == ND_LT)  emit("  setl %%al");
            if (n->kind == ND_LE)  emit("  setle %%al");
            emit("  movzbq %%al, %%rax");
            push_reg("%rax");
            return;
        case ND_AND: {
            int l = new_label();
            gen_val(n->lhs);
            pop_reg("%rax");
            emit("  cmpq $0, %%rax");
            emit("  je .Lfalse%d", l);
            gen_val(n->rhs);
            pop_reg("%rax");
            emit("  cmpq $0, %%rax");
            emit("  je .Lfalse%d", l);
            emit("  movq $1, %%rax");
            emit("  jmp .Lend%d", l);
            emit(".Lfalse%d:", l);
            emit("  movq $0, %%rax");
            emit(".Lend%d:", l);
            push_reg("%rax");
            return;
        }
        case ND_OR: {
            int l = new_label();
            gen_val(n->lhs);
            pop_reg("%rax");
            emit("  cmpq $0, %%rax");
            emit("  jne .Ltrue%d", l);
            gen_val(n->rhs);
            pop_reg("%rax");
            emit("  cmpq $0, %%rax");
            emit("  jne .Ltrue%d", l);
            emit("  movq $0, %%rax");
            emit("  jmp .Lend%d", l);
            emit(".Ltrue%d:", l);
            emit("  movq $1, %%rax");
            emit(".Lend%d:", l);
            push_reg("%rax");
            return;
        }
        case ND_CALL: {
            /* 引数を評価してレジスタにセット */
            int nargs = n->args->len;
            if (nargs > MAX_ARGS) die("too many arguments");
            for (int i = 0; i < nargs; i++)
                gen_val(n->args->data[i]);
            for (int i = nargs - 1; i >= 0; i--)
                pop_reg((char*)argregs[i]);
            /* スタックを16バイトアライン */
            if (stack_depth % 2 != 0) {
                emit("  subq $8, %%rsp");
                stack_depth++;
                emit("  movq $0, %%rax"); /* vararg用 */
                emit("  call %s", n->func_name);
                emit("  addq $8, %%rsp");
                stack_depth--;
            } else {
                emit("  movq $0, %%rax");
                emit("  call %s", n->func_name);
            }
            push_reg("%rax");
            return;
        }
        default:
            die("gen_val: unknown node kind %d", n->kind);
    }
}

static void gen_stmt(Node *n, int ret_label);

static void gen_stmt(Node *n, int ret_label) {
    switch (n->kind) {
        case ND_BLOCK:
            for (int i = 0; i < n->stmts->len; i++)
                gen_stmt(n->stmts->data[i], ret_label);
            return;
        case ND_EXPR_STMT:
            gen_val(n->lhs);
            pop_reg("%rax"); /* 結果を捨てる */
            return;
        case ND_VAR_DECL:
            if (n->init) {
                /* var x: T = init → x = init として代入 */
                Node *var_node = new_node(ND_VAR, n->line);
                var_node->var = n->var;
                var_node->type = n->var->type;
                Node *assign = new_node(ND_ASSIGN, n->line);
                assign->lhs = var_node;
                assign->rhs = n->init;
                assign->type = n->var->type;
                Node *stmt = new_node(ND_EXPR_STMT, n->line);
                stmt->lhs = assign;
                gen_stmt(stmt, ret_label);
            }
            return;
        case ND_RETURN:
            if (n->lhs) {
                gen_val(n->lhs);
                pop_reg("%rax");
            } else {
                emit("  movq $0, %%rax");
            }
            emit("  jmp .Lret%d", ret_label);
            return;
        case ND_IF: {
            int l = new_label();
            gen_val(n->cond);
            pop_reg("%rax");
            emit("  cmpq $0, %%rax");
            if (n->else_) {
                emit("  je .Lelse%d", l);
                gen_stmt(n->then_, ret_label);
                emit("  jmp .Lend%d", l);
                emit(".Lelse%d:", l);
                gen_stmt(n->else_, ret_label);
                emit(".Lend%d:", l);
            } else {
                emit("  je .Lend%d", l);
                gen_stmt(n->then_, ret_label);
                emit(".Lend%d:", l);
            }
            return;
        }
        case ND_WHILE: {
            int l = new_label();
            emit(".Lbegin%d:", l);
            gen_val(n->cond);
            pop_reg("%rax");
            emit("  cmpq $0, %%rax");
            emit("  je .Lend%d", l);
            gen_stmt(n->body, ret_label);
            emit("  jmp .Lbegin%d", l);
            emit(".Lend%d:", l);
            return;
        }
        default:
            die("gen_stmt: unknown stmt kind %d", n->kind);
    }
}

static void gen_func(Node *n) {
    int ret_label = new_label();

    /* 関数プロローグ */
    emit(".globl %s", n->name);
    emit("%s:", n->name);
    emit("  pushq %%rbp");
    emit("  movq %%rsp, %%rbp");

    /* ローカル変数領域確保 (16バイトアライン) */
    int lsize = n->local_size;
    if (lsize % 16 != 0) lsize += 16 - (lsize % 16);
    if (lsize > 0)
        emit("  subq $%d, %%rsp", lsize);

    /* 引数をスタックフレームにコピー (パラメータはローカル変数として扱う) */
    for (int i = 0; i < n->params->len && i < MAX_ARGS; i++) {
        Var *v = n->params->data[i];
        int sz = type_size(v->type);
        if (sz == 1)      emit("  movb %sb, %d(%%rbp)", argregs[i]+1, v->offset);
        else if (sz == 2) emit("  movw %sw, %d(%%rbp)", argregs[i]+1, v->offset);
        else if (sz == 4) emit("  movl %sd, %d(%%rbp)", argregs[i]+1, v->offset);
        else              emit("  movq %s, %d(%%rbp)", argregs[i], v->offset);
    }

    /* 本体生成 */
    stack_depth = 0;
    gen_stmt(n->func_body, ret_label);

    /* エピローグ */
    emit(".Lret%d:", ret_label);
    emit("  movq %%rbp, %%rsp");
    emit("  popq %%rbp");
    emit("  ret");
    emit("");
}

/* ================================================================
   メインドライバ
   ================================================================ */

static char *read_file(const char *path) {
    FILE *f = fopen(path, "r");
    if (!f) { perror(path); exit(1); }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    rewind(f);
    char *buf = xmalloc(sz + 1);
    fread(buf, 1, sz, f);
    buf[sz] = '\0';
    fclose(f);
    return buf;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: tinyc <source.tc> [-o output.s]\n");
        return 1;
    }

    const char *infile = argv[1];
    const char *outfile = "out.s";
    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i+1 < argc)
            outfile = argv[++i];
    }

    char *src = read_file(infile);
    str_literals = new_vec();

    Vec *nodes = parse_program(src);

    out = fopen(outfile, "w");
    if (!out) { perror(outfile); return 1; }

    /* コードセクション */
    fprintf(out, "  .section .note.GNU-stack,\"\",@progbits\n");
    emit("  .text");
    emit("");

    /* グローバル変数 (BSS/DATAセクション) */
    int has_globals = 0;
    for (int i = 0; i < nodes->len; i++) {
        Node *n = nodes->data[i];
        if (n->kind == ND_GLOBAL_VAR) { has_globals = 1; break; }
    }
    if (has_globals) {
        emit("  .data");
        for (int i = 0; i < nodes->len; i++) {
            Node *n = nodes->data[i];
            if (n->kind == ND_GLOBAL_VAR) {
                emit(".globl %s", n->var->label);
                emit("%s:", n->var->label);
                int sz = type_size(n->var->type);
                if (sz <= 0) sz = 8;
                if (n->ival != 0)
                    emit("  .quad %lld", (long long)n->ival);
                else
                    emit("  .zero %d", sz);
            }
        }
        emit("");
        emit("  .text");
        emit("");
    }

    for (int i = 0; i < nodes->len; i++) {
        Node *n = nodes->data[i];
        if (n->kind == ND_FUNC)
            gen_func(n);
    }

    /* データセクション (文字列リテラル) */
    if (str_literals->len > 0) {
        emit("  .section .rodata");
        for (int i = 0; i < str_literals->len; i++) {
            StrLiteral *sl = str_literals->data[i];
            emit(".LC%d:", sl->label);
            /* バイト列として出力 */
            fprintf(out, "  .ascii \"");
            for (int j = 0; j < sl->len; j++) {
                unsigned char c = (unsigned char)sl->data[j];
                if (c == '"' || c == '\\')
                    fprintf(out, "\\%c", c);
                else if (c < 32 || c >= 127)
                    fprintf(out, "\\x%02x", c);
                else
                    fprintf(out, "%c", c);
            }
            fprintf(out, "\\x00\"\n");
        }
    }

    fclose(out);
    return 0;
}