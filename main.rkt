#lang racket

(require racket/cmdline)
(require racket/pretty)
(require "lang/datastore.rkt")
(require "compiler.rkt")

(define source-file-path
  (command-line
   #:args (path)
   path))

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
