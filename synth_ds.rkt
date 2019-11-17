#lang rosette

(require "lang/datastore.rkt")
(require redex)

(define-symbolic b boolean?)
(define (my-prog val)
  (term ((ife x (0) (1)) () ((x ,val)))))

; Unfortunately it's not so straightforward to use Rosette
;
; (apply-reduction-relation* red (my-prog b))
;
; redex doesn't know how to reduce the variable value because of the symbolic value

; (define sol
;   (solve (assert (= (term (0 () ((x, #t)))) (apply-reduction-relation* red (my-prog b))))))
