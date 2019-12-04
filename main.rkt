#lang rosette

(require racket/cmdline)
(require racket/pretty)
(require rosette/lib/synthax)
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


(define t1 (current-milliseconds))
(define t2 (current-milliseconds))

;; Demo
(displayln "___________\n")
(println "Original Program")
(pretty-print program)
(displayln "___________\n")

(println "Mechanical translation")
(set! t1 (current-milliseconds))
(pretty-print (v1v2trans program))
(set! t2 (current-milliseconds))
(display "\n")
(displayln "--TIME--") (- t2 t1)
(display "\n")
(displayln "--Number of AST Nodes--") (displayln (number-nodes (v1v2trans program)))

(displayln "___________\n")

(println "Auto-Sketching translation")
(define asprog (v1v2trans/s program))
(set! t1 (current-milliseconds))
;(do-synth 5 asprog '())
(do-synth '(void) asprog '())
;(do-synth 0 asprog '())
;
;
;
(set! t2 (current-milliseconds))
(display "\n")
(displayln "--TIME--") (- t2 t1)
(display "\n")
(displayln "--Number of AST Nodes--")
;(displayln (syn-count-node 5 asprog '()))
(displayln (syn-count-node 0 asprog '()))
;
;
;
;

(displayln "___________\n")

(println "Manual-Sketching translation")
(set! t1 (current-milliseconds))
;(do-synth 5 sk1 '())
(do-synth 0 sk2 '())
;
;
;
;
(set! t2 (current-milliseconds))
(display "\n")
(displayln "--TIME--") (- t2 t1)
(display "\n")
(displayln "--Number of AST Nodes--")
;(displayln (syn-count-node 5 sk1 '()))
(displayln (syn-count-node 0 sk2 '()))
;
;
;
;
(display "\n")
