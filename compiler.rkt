#lang racket

(provide translate-core translate-ds-v1-v2 compose-translate run-translate)

(require (only-in rosette/lib/synthax ?? choose))
(require "lang/core.rkt")
(require "lang/datastore.rkt")

; Recursive descent into subexpressions
(define (translate-core f prog)
  (match prog
    [`(let ,(? symbol? x) ,expr) `(let ,x ,(f expr))]
    [`(,(? binop? op) ,e ...) `(,op ,(f e) ...)]
    [`(block ,e ...) `(block ,@(map f e))]
    [`(if ,guard ,texpr ,fexpr) `(if ,(f guard) ,(f texpr) ,(f fexpr))]
    [_ prog]))

; Translate datastore v1 into datastore v2
(define (translate-ds-v1-v2 f prog)
  (match prog
    [`(store ,key ,val) `(block (if (contains ,key) (del ,key) (void)) (store ,key ,val))]
    [`(del ,key) `(if (contains ,key) (del ,key) (void))]
    [`(get ,key) `(if (contains ,key) (get ,key) #f)]
    [_ (f prog)]
    ))

; Compose translators from right to left (i.e. t2 is executed before t1)
(define (compose-translate t1 t2)
  (lambda (f prog)
    (define (ct p) (t2 f prog))
    (t1 ct prog)))

(define (run-translate translator prog)
  (define (trans prog)
    (translator trans prog))
  (trans prog))

(module+ test
  (require rackunit)
  (define (v1v2trans prog)
    (run-translate (compose-translate translate-ds-v1-v2 translate-core) prog))
  (check-equal? (v1v2trans '(del 5))
                '(if (contains 5) (del 5) (void)))
  (check-equal? (v1v2trans '(block (del 5)))
                '(block (if (contains 5) (del 5) (void))))
  (check-equal? (v1v2trans '(block (store x 10) (get x)))
                '(block (block (if (contains x) (del x) (void)) (store x 10))
                        (if (contains x) (get x) #f)))
  )
