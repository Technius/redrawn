#lang rosette

(require "core.rkt")

(provide datastore-eval datastore-eval/rules datastore-eval-v2 datastore-eval/rules-v2)

(define (store-contains store key)
  (not (eq? (assoc key store) #f)))

(define (store-del store key)
  (remove key store (lambda (k p) (equal? (car p) k))))

(define (store-put store key val)
  (cons (cons key val) store))

(define (datastore-eval/rules cont f expr ctx vars)
  (match expr
    [`(store ,e1 ,e2)
     (match-define (list (list key val) nctx nvars)
       (core-eval/fold f (list e1 e2) ctx vars))
     `((void) ,(store-put (store-del nctx key) key val) ,nvars)]
    [`(del ,e)
     (match-define (list key nctx nvars) (f e ctx vars))
     `((void) ,(store-del nctx key) ,nvars)]
    [`(get ,e)
     (match-define (list key nctx nvars) (f e ctx vars))
     (for/all ([result (assoc key nctx)])
       (match result
         [(cons key v) `(,v ,nctx ,nvars)]
         [#f `((void) ,nctx ,nvars)]))]
    [`(contains ,e)
     (match-define (list key nctx nvars) (f e ctx vars))
     `(,(store-contains nctx key) ,nctx ,nvars)]
    [_ (cont f expr ctx vars)]))

(define (datastore-eval/rules-v2 cont f expr ctx vars)
  (match expr
    [`(store ,e1 ,e2)
     (match-define (list (list key val) nctx nvars)
       (core-eval/fold f (list e1 e2) ctx vars))
     (if (store-contains nctx key)
         (error 'datastore "Already contains datastore key: ~e" key)
         `((void) ,(store-put (store-del nctx key) key val) ,nvars))]
    [`(del ,e)
     (match-define (list key nctx nvars) (f e ctx vars))
     (if (store-contains nctx key)
         `((void) ,(store-del nctx key) ,nvars)
         (error 'datastore "No such datastore key: ~e" key))]
    [`(get ,e)
     (match-define (list key nctx nvars) (f e ctx vars))
     (for/all ([result (assoc key nctx)])
       (match result
         [(cons key v) `(,v ,nctx ,nvars)]
         [#f (error 'datastore "No such datastore key: ~e" key)]))]
    [_ (cont f expr ctx vars)]))

(define (datastore-eval cont f expr ctx vars)
  ((compose-interpreter datastore-eval/rules core-eval) cont f expr ctx vars))

(define (datastore-eval-v2 cont f expr ctx vars)
  ((compose-interpreter datastore-eval/rules-v2 datastore-eval) cont f expr ctx vars))

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
  (check-equal? (run '(block (store 2 0) (get (+ 1 1))))
                0)
  (check-equal? (run '(block (store 2 0) (contains (+ 1 1))))
                #t)
  (check-equal? (run '(block (store (+ 1 1) (+ 2 3)) (get 2)))
                5)
  (check-equal? (run '(block (store (+ 1 1) 0) (del 2) (contains (- 3 1))))
                #f)
  (check-exn exn:fail? (lambda () (run2 '(block (store 1 10)
                                                (store 1 10)))))
  (check-exn exn:fail? (lambda () (run2 '(get 1)))))
