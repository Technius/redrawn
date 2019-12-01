#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/angelic)
(require "lang/core.rkt")
(require "lang/datastore.rkt")

(provide ast??)

; Arbitrary AST node hole
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


(module+ test
  (require (prefix-in opt: "optimize.rkt"))
  (define-symbolic i integer?)

  (define (run p)
    (define interp
      (compose-interpreter* rosette-eval
                            datastore-eval/rules
                            core-eval))
    (run-program interp p))

  (define (do-synth output sketch vars)
    (define sol (synthesize
                 #:forall vars
                 #:guarantee (assert (equal? output (run sketch)))))
    (if (sat? sol)
        (begin
          (println "Sat")
          (define raw (evaluate sketch sol))
          (displayln (format "Raw: ~a" raw))
          (opt:optimize raw))
        (println "Unsat")))

  (println "Sketch 1")
  (define sk1
    `(block ,(ast?? (list i '(void)) 1)
            (get ,i)))
  (do-synth 5 sk1 (list i))

  (define-symbolic x integer?)

  (println "Sketch 2")
  (define sk2 `(block ,(ast?? (list x i '(void)) 2)))
  (do-synth (+ x (+ i i)) sk2 (list i x))

  (println "Sketch 3")
  (define sk3 (ast?? (list i x '(void)) 2))
  (do-synth (if (equal? i 2) i x) sk3 (list i x))
  )
