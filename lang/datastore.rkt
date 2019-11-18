#lang rosette

(require "core.rkt")

(provide datastore-eval datastore-eval/rules)

(define (store-del store key)
  (remove key store (lambda (k p) (equal? (car p) k))))

(define (store-put store key val)
  (cons (cons key val) store))

(define (datastore-eval/rules f expr ctx vars)
  (match expr
    [`(store ,key ,val)
     `((void) ,(store-put (store-del ctx key) key val) ,vars)]
    [`(del ,key)
     `((void) ,(store-del ctx key) ,vars)]
    [`(get ,key)
     (match (assoc key ctx)
       [(cons key v) `(,v ,ctx ,vars)]
       [#f `((void) ,ctx ,vars)])]
    [`(contains ,key)
     `(,(not (eq? (assoc key ctx) #f)) ,ctx ,vars)]
    [_ (f expr ctx vars)]))

(define (datastore-eval f expr ctx vars)
  (define (cf expr ctx vars) (core-eval f expr ctx vars))
  (datastore-eval/rules cf expr ctx vars))

(module+ test
  (require rackunit)
  (define (run prog) (run-program datastore-eval prog))
  (check-equal? (run '(block (store 1 10)
                             (contains 1)))
                #t)
  (check-equal? (run '(block (store 1 10)
                             (get 1)))
                10)
  (check-equal? (run '(block (store 1 10)
                             (del 1)
                             (get 1)))
                '(void)))
