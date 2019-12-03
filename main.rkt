#lang racket

(require racket/cmdline)
(require racket/pretty)
(require "lang/datastore.rkt")
(require "compiler.rkt")

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

(displayln "Datastore V1:")
(pretty-print program)

; TODO: generate sketch and then synthesize
(define translator
  (compose-translate translate-ds-v1-v2 translate-core))

(displayln "Translate...")
(define program-v2 (run-translate translator program))

(displayln "Datastore V2:")
(pretty-print program-v2)
