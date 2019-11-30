#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/angelic)
(require "lang/core.rkt")
(require "lang/datastore.rkt")

(define (ast?? terminals max-depth)
  (define (go d)
    (define (recurse) (go (- d 1)))
    (define t (cons (??) terminals))
    (if (<= d 0)
        (apply choose* t)
        (begin
          (define e1 (recurse))
          (define e2 (recurse))
          (define e3 (recurse))
          (define ast-holes
            (append
             t
             (list
              `(,(choose* 'get 'del 'contains) ,e1)
              `(,(choose* '+ '- '&& '|| 'store) ,e1 ,e2)
              `(if ,e1 ,e2 ,e3)
              )))
          (apply choose* ast-holes))))
  (go max-depth))


(define-symbolic i integer?)

(define (run p)
  (define interp
    (compose-interpreter* rosette-eval
                          datastore-eval/rules
                          core-eval))
  (run-program interp p))

(define p1 `(block (store ,i 5) (get ,i)))
(define sketch
  `(block ,(ast?? (list i) 1)
          (get ,i)))

(define sol (synthesize
             #:forall (list i)
             #:guarantee (assert (equal? 5 (run sketch)))))
(evaluate sketch sol)

(define-symbolic x integer?)
(define sk2 `(block ,(ast?? (list x i) 2)))

(define sol2 (synthesize
             #:forall (list i x)
             #:guarantee (assert (equal? (+ x (+ i i)) (run sk2)))))
(when (sat? sol2)
  (println "Sat")
  (evaluate sk2 sol2))
