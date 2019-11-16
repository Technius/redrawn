#lang racket

(require "datastore/syntax.rkt")

(define program (syntax->datum (ast-from-file "./examples/datastore/prog1.txt")))
(display "Program: ")
(display program)
(newline)
(display "Output: ")
(display (eval-program (cdr program) #hash()))
(newline)
