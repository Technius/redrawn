#lang rosette

(define-symbolic b boolean?)
(define-symbolic i integer?)

(require "lang/core.rkt")

(solve (assert (= 10  (run-program `(block (let x ,i) (+ x 10))))))
