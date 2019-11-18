#lang rosette

(require rosette/lib/synthax)

(define-symbolic b boolean?)
(define-symbolic i integer?)

(require "lang/core.rkt")

(define (go)
  (solve (assert (= 10 (run-program core-eval `(block (let x ,i) (+ x 10)))))))
(go)

; Example program synthesis
(define (prog i) `(- ,[choose i 0] ,i))

(define sy
  (synthesize
   #:forall (list i)
   #:guarantee
   (assert (equal? 0 (run-program core-eval (prog i))))))
(print-forms sy)
