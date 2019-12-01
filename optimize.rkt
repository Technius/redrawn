#lang racket

(require rosette/base/core/type)
(require "lang/core.rkt")

(provide optimize)

; Constant-folding optimizer
(define (optimize prog)
  (match prog
    [`(+ ,e1 ,e2)
     (match `(,(optimize e1) ,(optimize e2))
       [(list o1 0) o1]
       [(list 0 o2) o2]
       [(list o1 o2) `(+ ,o1 ,o2)])]
    [`(- ,e1 ,e2)
     (match `(,(optimize e1) ,(optimize e2))
       [(list o1 0) o1]
       [(list o1 o2) `(- ,o1 ,o2)])]
    [`(,(? binop? op) ,e ...) `(,op ,@(map optimize e))]
    [`(if ,e1 ,e2 ,e3)
     (define guard (optimize e1))
     (match guard
       [#t (optimize e2)]
       [#f (optimize e3)]
       [_ `(if ,guard ,(optimize e2) ,(optimize e3))])]
    [`(block ,e ...)
     (define opt (map optimize e))
     (define fe (filter (lambda (p) (not (equal? p '(void)))) opt))
     (match (length fe)
       [0 '(void)]
       [1 (car fe)]
       [_ `(block ,@fe)])]
    [_ prog]))

(module+ test
  (require rackunit)
  (check-equal? (optimize '(if #t 1 2)) 1)
  (check-equal? (optimize '(if #f 1 2)) 2)
  (check-equal? (optimize '(+ 1 0)) 1)
  (check-equal? (optimize '(&& (== (+ 1 0) (- 1 0)))) #t)
  (check-equal? (optimize '(if (== (+ 1 0) 1) 1 2)) 1)
  (check-equal? (optimize '(block (if #t (void) 1) 2 (void) 3)) '(block 2 3))
  )
