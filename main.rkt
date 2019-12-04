#lang rosette

(require racket/cmdline)
(require racket/pretty)
(require rosette/lib/synthax)
(require (only-in rosette/base/core/reflect symbolics))
(require "lang/core.rkt")
(require "lang/datastore.rkt")
(require "compiler.rkt")
(require "synthesis.rkt")

(define init-vars (make-parameter '()))

(define source-file-path
  (command-line
   #:multi
   [("-v" "--var") vv
                    "Define initial variables"
                    (init-vars (cons vv (init-vars)))]
   #:args (path)
   path))

; Parse initial variables
(define kv-pairs
  (map (lambda (s) (string-split s #px":")) (init-vars)))
(define vars
  (for/list ([p kv-pairs])
    (cond
      [(equal? 2 (length p))
       (define v (string->symbol (car p)))
       (list v (cadr p))]
      [else
       (printf "Error: invalid variable: ~a\n" p)
       (exit 1)]
      )))
(define inputs (create-input-vars vars))

(define program
  (read (open-input-file source-file-path #:mode 'text)))

(define-symbolic i integer?)
(define-symbolic x integer?)


;; sketch 1
(define sk1 `(block ,(ast?? (list i '(void)) 1) (get ,i)))
(define sk2 `(block (store 0 0) 
              (store 1 1)
              (del 0)
              ,(ast?? (list i x) 1)
              (if (contains 0) (get 0) (get 1))))
(define sk3 `(block
    ,(ast?? (list i x) 1)
    (store 10 10)
    (if (contains 0)
        (store 1 1)
        (store 2 2))
    (if (contains 1)
        (if (|| (contains 10) (contains 2))
            (del 2)
            (del 1))
            (store 10 10))
    (get 0)))

(define (benchmark proc)
  (define t1 (current-milliseconds))
  (define ret (proc))
  (define t2 (current-milliseconds))
  (values ret (- t2 t1)))

(define (run-benchmark proc)
  (let-values ([(ret delta) (benchmark proc)])
    (printf "TIME: ~a ms\n" delta)
    ret))

(define (demo prog)
  (displayln "___________\n")
  (displayln "Original program")
  (pretty-print prog)

  (displayln "___________\n")
  (displayln "Mechanical translation")
  (define m-prog
    (run-benchmark
     (thunk
      (let ([p (v1v2trans prog)])
        (pretty-print p)
        p))))
  (printf "Number of AST nodes: ~a\n" (length (flatten m-prog)))

  (displayln "___________\n")
  (displayln "Auto sketching translation")
  (define asketch (v1v2trans/s prog))
  (define as-prog
    (run-benchmark
     (thunk
      (do-synth (run prog #:init-vars inputs)
                asketch (symbolics inputs)
                #:init-vars inputs))))
  (printf "Number of AST nodes: ~a\n" (length (flatten as-prog)))

  (displayln "___________\n")
  (displayln "Manual sketching translation")
  ; TODO: Load manual sketch
  (define msketch '(block))
  (define ms-prog
    (run-benchmark
     (thunk
      (do-synth (run prog #:init-vars inputs)
                msketch (symbolics inputs)
                #:init-vars inputs))))
  (printf "Number of AST nodes: ~a\n" (length (flatten ms-prog)))

  (displayln "___________\n")
  (displayln "Full program synthesis")
  (define terminals
    (for/list ([i inputs])
      (match i
        [(cons x v) x]
        [x x])))
  (define f-prog
    (run-benchmark
     (thunk
      (do-synth (run prog #:init-vars inputs)
                (ast?? terminals 2 0) (symbolics inputs)
                #:init-vars inputs))))
  (printf "Number of AST nodes: ~a\n" (length (flatten f-prog)))
  )

(demo program)
