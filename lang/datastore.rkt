#lang rosette

(require "core.rkt")

(provide datastore-eval datastore-eval/rules datastore-eval-v2 datastore-eval/rules-v2)

(define (store-contains store key)
  (not (eq? (assoc key store) #f)))

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
    [`(contains ,key) `(,(store-contains ctx key) ,ctx ,vars)]
    [_ (f expr ctx vars)]))

(define (datastore-eval/rules-v2 f expr ctx vars)
  (match expr
    [`(store ,key ,val)
     (if (store-contains ctx key)
         (error 'datastore "Already contains datastore key: ~e" key)
         `((void) ,(store-put (store-del ctx key) key val) ,vars))]
    [`(del ,key)
     (if (store-contains ctx key)
         `((void) ,(store-del ctx key) ,vars)
         (error 'datastore "No such datastore key: ~e" key))]
    [`(get ,key)
     (match (assoc key ctx)
       [(cons key v) `(,v ,ctx ,vars)]
       [#f (error 'datastore "No such datastore key: ~e" key)])]
    [_ (f expr ctx vars)]))

(define (datastore-eval f expr ctx vars)
  ((compose-interpreter datastore-eval/rules core-eval) f expr ctx vars))

(define (datastore-eval-v2 f expr ctx vars)
  ((compose-interpreter datastore-eval/rules-v2 datastore-eval) f expr ctx vars))

(module+ test
  (require rackunit)
  (define (run prog) (run-program datastore-eval prog))
  (define (run2 prog) (run-program datastore-eval-v2 prog))
  (check-equal? (run '(block (store 1 10)
                             (contains 1)))
                #t)
  (check-equal? (run '(block (store 1 10)
                             (get 1)))
                10)
  (check-equal? (run '(block (store 1 10)
                             (del 1)
                             (get 1)))
                '(void))

  (check-equal? (run2 '(block (store 1 10)
                              (del 1)
                              (store 1 20)
                              (get 1)))
                20)
  (check-exn exn:fail? (lambda () (run2 '(block (store 1 10)
                                                (store 1 10)))))
  (check-exn exn:fail? (lambda () (run2 '(get 1)))))
