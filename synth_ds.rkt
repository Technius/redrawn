#lang rosette

(require rosette/lib/synthax)

(define-symbolic b boolean?)
(define-symbolic i integer?)

(require "lang/core.rkt")
(require "lang/datastore.rkt")

(define (go)
  (solve (assert (= 10 (run-program core-eval `(block (let x ,i) (+ x 10)))))))
(go)

; Example program synthesis
(define (prog i) `(if (== (- ,[choose i 0] ,i) 0) 10 20))

(define sy
  (synthesize
   #:forall (list i)
   #:guarantee
   (assert (equal? 10 (run-program core-eval (prog i))))))
(print-forms sy)

(define dprog
  `(block (if ,b (let x ,i) (let x #f))
          (if ,b (+ x 5) #t)))

(run-program datastore-eval dprog)
