#lang racket

(provide translate-core translate-ds-v1-v2 compose-translate run-translate)

(require (only-in rosette/lib/synthax ?? choose))
(require "lang/core.rkt")
(require "lang/datastore.rkt")

; Recursive descent into subexpressions
(define (translate-core cont f prog)
  (match prog
    [`(let ,(? symbol? x) ,expr) `(let ,x ,(f expr))]
    [`(,(? binop? op) ,e ...) `(,op ,(f e) ...)]
    [`(block ,e ...) `(block ,@(map f e))]
    [`(if ,guard ,texpr ,fexpr) `(if ,(f guard) ,(f texpr) ,(f fexpr))]
    [_ prog]))

; Translate datastore v1 into datastore v2
(define (translate-ds-v1-v2 cont f prog)
  (match prog
    [`(store ,key ,val) `(block (if (contains ,(f key)) (del ,(f key)) (void)) (store ,(f key) ,(f val)))]
    [`(del ,key) `(if (contains ,(f key)) (del ,(f key)) (void))]
    [`(get ,key) `(if (contains ,(f key)) (get ,(f key)) #f)]
    [_ (cont f prog)]
    ))

; Compose translators from right to left (i.e. t2 is executed before t1)
(define (compose-translate t1 t2)
  (lambda (cont f prog)
    (define (ct f p) (t2 cont f prog))
    (t1 ct f prog)))

(define (run-translate translator prog)
  (define (trans prog)
    (translator identity trans prog))
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
  (check-equal? (v1v2trans '(block (store 1 2) (store 2 3) (del (get 1))))
                '(block
                  (block (if (contains 1) (del 1) (void)) (store 1 2))
                  (block (if (contains 2) (del 2) (void)) (store 2 3))
                  (if (contains (if (contains 1) (get 1) #f))
                      (del (if (contains 1) (get 1) #f))
                      (void))))
  )
