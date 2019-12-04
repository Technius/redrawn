#lang rosette

(require racket/format)
(require rosette/lib/synthax)
(require rosette/lib/angelic)
(require (only-in racket/base [equal? racket:equal?]))
(require "lang/core.rkt")
(require "lang/datastore.rkt")
(require "compiler.rkt")

(provide ast?? create-input-vars
        v1v2trans v1v2trans/s run
        do-synth syn-count-node)

; Arbitrary AST node hole
(define (ast?? terminals max-depth [block-length 0])
  (define bool-terms (filter boolean? terminals))
  (define (block-hole h)
    (if (> block-length 0)
        (list `(block ,@(build-list block-length (lambda (_) (h)))))
        '()))
  (define (go-boolean d [consts? #t])
    (define t
      (if consts? (cons (?? boolean?) bool-terms) bool-terms))
    (if (<= d 0)
        (apply choose* t)
        (begin
          (define e1 (go-boolean (- d 1) consts?))
          (define e2 (go-boolean (- d 1) consts?))
          (define ea1 (go (- d 1)))
          (define ea2 (go (- d 1)))
          (define ei1 (go-integer (- d 1)))
          (define ei2 (go-integer (- d 1)))
          (define holes
            (append
             (block-hole (thunk (go (- d 1))))
             (list
              `(contains ,ea1)
              `(,(choose* '&& '||) ,e1 ,e2)
              `(,(choose* '>= '<= '> '<) ,ei1 ,ei2)
              `(== ,ea1 ,ea2)
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
             (block-hole (thunk (go (- d 1))))
             (list
              `(,(choose* '+ '- '* '/) ,e1 ,e2)
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

(define (create-input-vars vars)
  (for/list ([p vars])
    (match p
      [`(,(? symbol? v) "integer")
       (define-symbolic* inputi integer?)
       (list v inputi)]
      [`(,(? symbol? v) "boolean")
       (define-symbolic* inputb boolean?)
       (list v inputb)]
      [_ p])))



(require (prefix-in opt: "optimize.rkt"))

(define (v1v2trans prog)
    (run-translate (compose-translate translate-ds-v1-v2 translate-core) prog))

(define (v1v2trans/s prog)
    (run-translate (compose-translate translate-ds-v1-v2/sketching translate-core) prog))

(define (run p #:init-vars [init-vars '()] #:init-ctx [init-ctx '()])
  (define interp
    (compose-interpreter* rosette-eval
                          datastore-eval/rules-v2
                          datastore-eval/rules
                          core-eval))
  (run-program interp p #:init-vars init-vars #:init-ctx init-ctx))

(define (do-synth output sketch vars #:init-vars [init-vars '()])
  (define init-store
    (build-list 10
                (lambda (i)
                  (define-symbolic* storei integer?)
                  `(,i ,storei))))
  (define sol
    (synthesize
     #:forall (append vars (symbolics init-store))
     #:guarantee
     (assert (equal? output (run sketch #:init-vars init-vars
                                 #:init-ctx init-store)))))
  (if (sat? sol)
      (begin
        (displayln "Sat")
        (define hole-vars (remove* vars (symbolics sketch) racket:equal?))
        (define solc (complete-solution sol hole-vars))
        (define raw (evaluate sketch solc))
        (printf "Raw: ~a\n" (pretty-format raw))
        (define opt (opt:optimize raw))
        (printf "Opt: ~a\n" (pretty-format opt))
        opt)
        (displayln "Unsat")))

(define (syn-count-node output sketch vars)
  (define sol (synthesize
                #:forall vars
                #:guarantee (assert (equal? output (run sketch)))))
  (if (sat? sol)
      (begin
        (define hole-vars (remove* vars (symbolics sketch) racket:equal?))
        (define solc (complete-solution sol hole-vars))
        (define raw (evaluate sketch solc))
        (define opt (opt:optimize raw))
        (displayln (number-nodes opt)))
        (displayln "Unsat")))
#|
;; Demo_1
(define p1 `(block (store ,i 5) (get ,i)))
(displayln "'''")
(println "Original Program 1")
(displayln p1)
(display "\n")

(println "Sketching translation 1")
(define st1 (v1v2trans/s p1))
(do-synth 5 st1 (list i))
(display "\n")

(println "Direct translation 1")
(displayln (v1v2trans p1))
(display "\n")

(println "Sketch 1")
(define sk1
  `(block ,(ast?? (list i) 1)
          (get ,i)))
(do-synth 5 sk1 (list i))
(display "\n")

(println "Summary 1")
(display "Number of AST nodes in direct translated program - ") (displayln (number-nodes (v1v2trans p1)))
(display "Number of AST nodes in synthesized program - ") (syn-count-node 5 sk1 (list i))
(displayln "'''\n")


;; Demo_2
(define p2 `(block (store 0 0) (store 1 1) (del 0) (del 1) (if (contains 0) (get 0) (get 1))))
(displayln "'''")
(println "Original Program 2")
(displayln p2)
(display "\n")

(println "Direct translation 2")
(displayln (v1v2trans p2))
(display "\n")

(println "Sketching translation 2")
(define st2 (v1v2trans/s p2))
(do-synth '(void) st2 '())
(display "\n")

(println "Sketch 2")
(define sk2 `(block (store 0 0) 
              (store 1 1)
              (del 0)
              ,(ast?? (list i x) 1)
              (if (contains 0) (get 0) (get 1))))
(do-synth '(void) sk2 (list i x))
(display "\n")

(println "Summary 2")
(display "Number of AST nodes in direct translated program - ") (displayln (number-nodes (v1v2trans p2)))
(display "Number of AST nodes in synthesized program - ") (syn-count-node '(void) sk2 (list i x))
(displayln "'''\n")


;; Demo_3
(define p3
    (let ([n 0])
    `(block
        (store 0 10)
        (store 1 100)
        (while (< n 3)
            (del n)
            (set! n (+ n 1)))
        (get 2))))
(displayln "'''")
(println "Original Program 3")
(displayln p3)
(display "\n")

(println "Direct translation 3")
(displayln (v1v2trans p3))
(display "\n")

(println "Sketch 3-1")
(define sk3 
    (let ([n 3])
    `(block
        ,(ast?? (list i x) 2)
        (while (< n 0)
            (del n)
            (set! n (- n 1)))
        (get 0))))
(do-synth 10 sk3 (list i x))
(display "\n")

(println "Sketch 3-2")
(define sk3-2 
    (let ([n 3])
    `(block
        ,(ast?? (list i x) 1)
        (store 1 100)
        (while (< n 0)
            (del n)
            (set! n (- n 1)))
        (get 0))))
(do-synth 10 sk3 (list i x))
(display "\n")

(println "Summary 3")
(display "Number of AST nodes in direct translated program - ") (displayln (number-nodes (v1v2trans p3)))
(display "Number of AST nodes in synthesized program - ") (syn-count-node 10 sk3-2 (list i))
(displayln "'''\n")


;; Demo_4
(define p4 `(block
              (store 0 0)
              (store 10 10)
              (if (contains 0)
                (store 1 1)
                (store 2 2))
              (if (contains 1)
                (if ((contains 10) || (contains 2))
                  (del 2)
                  (del 1))
                (store 10 10))
              (get 0)))
(displayln "'''")
(println "Original Program 4")
(displayln p4)
(display "\n")

(println "Direct translation 4")
(displayln (v1v2trans p4))
(display "\n")

(println "Sketch 4-1")
(define sk4 `(block
                ,(ast?? (list i x) 1)
                (store 10 10)
                (if (contains 0)
                  (store 1 1)
                  (store 2 2))
                (if (contains 1)
                  (if ((contains 10) || (contains 2))
                    (del 2)
                    (del 1))
                  (store 10 10))
                (get 0)))
(do-synth 0 sk4 (list i x))
(display "\n")

(println "Sketch 4-2")
;; Program is same as p4, except that the expected output is (get 2)
;; sk4-2 has two holes
(define sk4-2 `(block
                ,(ast?? (list i x) 1)
                (store 10 10)
                (if (contains 0)
                  (store 1 1)
                  ,(ast?? (list i x) 1))
                (if (contains 1)
                  (if ((contains 10) || (contains 2))
                    (del 2)
                    (del 1))
                  (store 10 10))
                (get 2)))
(do-synth '(void) sk4-2 (list i x))
(display "\n")

(println "Summary 4")
(display "Number of AST nodes in direct translated program - ") (displayln (number-nodes (v1v2trans p4)))
(display "Number of AST nodes in synthesized program - ") (syn-count-node 0 sk4 (list i x))
(displayln "'''\n")
|#
#|
;; Demo_5
(define p5 `(block))

(displayln "'''")
(println "Original Program 5")
(displayln p5)
(display "\n")

(println "Direct translation 5")
(displayln (v1v2trans p5))
(display "\n")




;; tests
(println "Sketch 2")
(define sk2 `(block ,(ast?? (list x i '(void)) 2)))
(do-synth (+ x (+ i i)) sk2 (list i x))
(displayln "'''\n")

(println "Sketch 3")
(define sk3 (ast?? (list i x '(void)) 2))
(do-synth (if (equal? i 2) i x) sk3 (list i x))
(display "\n")
|#
