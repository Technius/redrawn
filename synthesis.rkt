#lang rosette

(require rosette/lib/synthax)
(require "lang/core.rkt")
(require "lang/datastore.rkt")

; Holes for datastore rules
(define-synthax (datastore=> e1 e2 k)
  #:base (choose e1 e2)
  #:else (choose
          e1 e2
          `(get ,(datastore=> e1 e2 (- k 1)))
          `(del ,(datastore=> e1 e2 (- k 1)))
          `(contains ,(datastore=> e1 e2 (- k 1)))
          `(store ,(datastore=> e1 e2 (- k 1)) ,(datastore=> e1 e2 (- k 1)))))

(define-symbolic i integer?)

(define (run p)
  (define interp
    (compose-interpreter* rosette-eval
                          datastore-eval/rules
                          core-eval))
  (run-program interp p))

(define p1 `(block (store ,i 5) (get ,i)))
(define sketch
  `(block ,(datastore=> 5 i 1)
          (get ,i)))

(define sol (synthesize
             #:forall (list i)
             #:guarantee (assert (equal? 5 (run sketch)))))
(print-forms sol)
