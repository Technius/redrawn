#lang racket

(provide (all-defined-out) (except-out get-string-token))

(require brag/support)
(require br-parser-tools/lex)
(require (prefix-in re: br-parser-tools/lex-sre))
(require "grammar.rkt")

(define get-string-token
  (lexer
   ;; Function based on br-parser-tools/example/read.rkt
   [(re:~ #\\ #\") (cons (car (string->list lexeme)) (get-string-token input-port))]
   [(re:: #\\ #\\) (cons #\\ (get-string-token input-port))]
   [(re:: #\\ #\") (cons #\" (get-string-token input-port))]
   [#\" null]))

(define (tokenize ip)
  (port-count-lines! ip)
  (define ds-lexer
    (lexer-src-pos
     ;; Syntactic elements
     [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
     [";" (token ";" lexeme)]
     ["(" (token "(" lexeme)]
     [")" (token ")" lexeme)]
     [(eof) (void)]
     ;; Keywords
     [(re:or "store" "del") (string->symbol lexeme)]
     [(re:or "true" "false") (token 'BOOL (eq? lexeme "true"))]
     ;; Expressions
     [(re:or "+" "-") (token 'BINOP (string->symbol lexeme))]
     [(re:+ alphabetic) (token 'STRING lexeme)]
     [#\" (token 'STRING (list->string (get-string-token ip)))]
     [(re:+ numeric) (token 'INTEGER (string->number lexeme))]

     ))
  (define (next-token) (ds-lexer ip))
  next-token)

(define (ast-from-port input-port)
  (parse (tokenize input-port)))

(define (ast-from-string str)
  (ast-from-port (open-input-string str)))

(define (ast-from-file path)
  (ast-from-port (open-input-file path #:mode 'text)))

(define (eval-expr expr)
  (match expr
    [`(,(? integer? x)) x]
    [`(,(? boolean? b)) b]
    [`(,(? string? s)) s]
    [`((binop (,x) ,op (,y)))
     (match op
       ['+ (+ x y)]
       ['- (- x y)])]))

(define (eval-stmt stmt store)
  (match stmt
    [`(store ,k ,v) (hash-set store (eval-expr k) (eval-expr v))]))

(define (eval-program program init-store)
  (for/fold ([store init-store]) ([stmt program])
    (eval-stmt stmt store)))
