#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/angelic)
(require "lang/core.rkt")
(require "lang/datastore.rkt")

(define (ast?? terminals max-depth [block-length 0])
  (define (block-hole h)
    (if (> block-length 0)
        (list `(block ,@(build-list block-length (lambda (_) (h)))))
        '()))
  (define (go-boolean d [consts? #t])
    (define t
      (let [(t_ (filter boolean? terminals))]
        (if consts? (cons (?? boolean?) t_) t_)))
    (if (<= d 0)
        (apply choose* t)
        (begin
          (define e1 (go-boolean (- d 1) consts?))
          (define e2 (go-boolean (- d 1) consts?))
          (define holes
            (append
             (block-hole (lambda () (go (- d 1))))
             (list
              `(contains ,(go (- d 1)))
              `(,(choose* '&& '||) ,e1 ,e2)
              `(== ,(go (- d 1)) ,(go (- d 1)))
              `(if ,(go-boolean (- d 1) consts?) ,e1 ,e2))
             t))
          (apply choose* holes))))
  (define (go-integer d)
    (define t (cons (?? integer?) (filter integer? terminals)))
    (if (<= d 0)
        (apply choose* t)
        (begin
          (define e1 (go-integer (- d 1)))
          (define e2 (go-integer (- d 1)))
          (define holes
            (append
             (block-hole (lambda () (go (- d 1))))
             (list
              `(,(choose* '+ '-) ,e1 ,e2)
              `(if ,(go-boolean (- d 1)) ,e1 ,e2))
              t))
          (apply choose* holes))))
  (define (go d)
    (define (recurse) (go (- d 1)))
    (define t (cons (?? integer?) (cons (?? boolean?) terminals)))
    (if (<= d 0)
        (apply choose* t)
        (begin
          (define e1 (recurse))
          (define e2 (recurse))
          (define ast-holes
            (append
             (block-hole recurse)
             (list
              `(if ,(go-boolean (- d 1)) ,e1 ,e2)
              `(,(choose* 'get 'del 'contains) ,e1)
              `(,(choose* '== 'store) ,e1 ,e2)
              (go-boolean d)
              (go-integer d))
             t
             ))
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
  `(block ,(ast?? (list i '(void)) 1)
          (get ,i)))

(define sol (synthesize
             #:forall (list i)
             #:guarantee (assert (equal? 5 (run sketch)))))
(when (sat? sol)
  (println "Sat 1")
  (evaluate sketch sol))

(define-symbolic x integer?)
(define sk2 `(block ,(ast?? (list x i '(void)) 2)))

(define sol2 (synthesize
             #:forall (list i x)
             #:guarantee (assert (equal? (+ x (+ i i)) (run sk2)))))
(when (sat? sol2)
  (println "Sat 2")
  (evaluate sk2 sol2))

(define sk3 (ast?? (list i x '(void)) 2))

(define sol3 (synthesize
              #:forall (list i x)
              #:guarantee (assert (equal? (if (equal? i 2) i x) (run sk3)))))
(when (sat? sol3)
  (println "Sat 3")
  (evaluate sk3 sol3))
