#lang rosette

(require rosette/lib/synthax)
(require "lang/core.rkt")
(require "lang/datastore.rkt")

(define-synthax (datastore=> e1 e2 k)
  #:base (choose e1 e2)
  #:else (choose
          e1 e2
          `(get ,(datastore=> e1 e2 (- k 1)))
          `(del ,(datastore=> e1 e2 (- k 1)))
          `(contains ,(datastore=> e1 e2 (- k 1)))
          `(store ,(datastore=> e1 e2 (- k 1)) ,(datastore=> e1 e2 (- k 1))))
  )

(define-symbolic i integer?)

(define (run p) (run-program (compose-interpreter datastore-eval/rules core-eval) p))

(define p1 `(block (store ,i 0) (get ,i)))
(define sketch
  `(,(datastore=> 0 i 1)
    (get ,i)))

; TODO: handle symbolic union in evaluation rules

(synthesize
 #:forall (list i)
 #:guarantee (assert (equal? (run p1) (run sketch))))
