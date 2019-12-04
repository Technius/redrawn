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
(define synthesis-depth (make-parameter 1))
(define manual-sketch-file (make-parameter null))

(define source-file-path
  (command-line
   #:once-each
   [("-d" "--depth") d
                     "Synthesis depth"
                     (synthesis-depth d)]
   [("-s" "--manual-sketch") ms
                             "File of manual sketch (holes are specified with (ast?? ))"
                             (manual-sketch-file ms)]
   #:multi
   [("-v" "--var") vv
                    "Define initial variables"
                    (init-vars (cons vv (init-vars)))]
   #:args (path)
   path))

(define syn-depth (synthesis-depth))
(when (string? syn-depth)
  (set! syn-depth (string->number syn-depth))
  (when (not (integer? syn-depth))
    (raise-user-error 'main "Depth must be a positive integer")))

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

(define (demo prog ms-prog-file)
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
  (printf "Sketch: ~a\n" (pretty-format asketch))
  (define as-prog
    (run-benchmark
     (thunk
      (do-synth (run prog #:init-vars inputs)
                asketch (symbolics inputs)
                #:init-vars inputs))))
  (printf "Number of AST nodes: ~a\n" (length (flatten as-prog)))

  (define terminals
    (for/list ([i inputs])
      (match i
        [(cons x v) x]
        [x x])))

  (when (not (equal? null ms-prog-file))
    (displayln "___________\n")
    (displayln "Manual sketching translation")
    (define msketch
      (read (open-input-file ms-prog-file #:mode 'text)))
    (define (holify expr)
      (match expr
        [`(ast?? ,(? integer? d)) (ast?? terminals d)]
        [`(,e ...) `(,@(map holify e))]
        [_ expr]
        ))
    (set! msketch (holify msketch))
    (printf "Sketch: ~a\n" (pretty-format msketch #:mode 'display))
    (define ms-prog
      (run-benchmark
       (thunk
        (do-synth (run prog #:init-vars inputs)
                  msketch (symbolics inputs)
                  #:init-vars inputs))))
    (printf "Number of AST nodes: ~a\n" (length (flatten ms-prog)))
    )

  (displayln "___________\n")
  (displayln "Full program synthesis")
  (define f-prog
    (run-benchmark
     (thunk
      (do-synth (run prog #:init-vars inputs)
                (ast?? terminals syn-depth 0) (symbolics inputs)
                #:init-vars inputs))))
  (printf "Number of AST nodes: ~a\n" (length (flatten f-prog)))
  )

(demo program (manual-sketch-file))
