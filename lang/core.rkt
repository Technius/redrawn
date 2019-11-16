#lang racket

(require racket/function)
(require redex)
(require scribble/srcdoc)

(provide Core CoreE red)

;; Core expression language
(define-language
  Core
  (expr x
        number
        string
        boolean
        (binop expr expr ...)
        (ife expr expr expr))
  (binop + and or eq?)
  (x variable-not-otherwise-mentioned))

;; Core language evaluation contexts
(define-extended-language
  CoreE
  Core
  (Ee hole
      (binop v ... Ee expr ...)
      (ife Ee expr expr))
  (vars ((x v) ...)) ;; variables
  (v number string boolean))

(define-metafunction
  CoreE
  extend-vars : (vars x v) -> vars
  [(extend-vars vars x v) (cons (x  v) vars)])

(define-metafunction
  CoreE
  load-var : vars x -> v
  [(load-var ((x_1 v_1) ... (x v) (x_2 v_2) ...) x) v]
  [(load-var any_1 any_2) ,(error 'load-var "not found: ~e" (term x))])

;; Core language small-step operational semantics
(define red
  (reduction-relation
   CoreE
   #:domain (expr vars)
   ; Variable loads
   (--> [(in-hole Ee x) vars]
        [(in-hole Ee (load-var vars x)) vars])
   ; Binary operations
   (--> [(in-hole Ee (+ number ...)) vars]
        [(in-hole Ee ,(apply + (term (number ...)))) vars])
   (--> [(in-hole Ee (or boolean ...)) vars]
        [(in-hole Ee ,(ormap identity (term (boolean ...)))) vars])
   (--> [(in-hole Ee (and boolean ...)) vars]
        [(in-hole Ee ,(andmap identity (term (boolean ...)))) vars])
   (--> [(in-hole Ee (eq? v_1 v_2)) vars]
        [(in-hole Ee ,(apply eq? (term (v_1 v_2)))) vars])
   ; If expressions
   (--> [(in-hole Ee (ife #t expr_1 expr_2)) vars]
        [(in-hole Ee expr_1) vars])
   (--> [(in-hole Ee (ife #f expr_1 expr_2)) vars]
        [(in-hole Ee expr_2) vars])
   ))

(define-metafunction
  CoreE
  eval-expr : expr vars -> v or stuck
  [(eval-expr v vars) v]
  [(eval-expr expr vars)
   (eval-expr expr_next vars_next)
   (where ((expr_next vars_next)) ,(apply-reduction-relation red (term (expr vars))))]
  [(eval-expr any_1 any_2) stuck])

(module+ test
  (define my-vars (term ((x 10) (y 20) (z 30))))
  (test-->> red
            (term (x ,my-vars))
            (term (10 ,my-vars)))
  (test-->> red
            (term ((+ (+ 30 40) (+ 10 20)) ()))
            (term (100 ())))
  (test-->> red
            (term ((or #f #f #t) ()))
            (term (#t ())))
  (test-->> red
            (term ((and #f #f #t) ()))
            (term (#f ())))
  (test-->> red
            (term ((eq? 1 1) ()))
            (term (#t ())))
  (test-->> red
            (term ((ife #t 1 0) ()))
            (term (1 ())))
  (test-->> red
            (term ((ife #f 1 0) ()))
            (term (0 ())))
  (test-->> red
            (term ((ife (eq? x y) x (+ x y)) ,my-vars))
            (term (30 ,my-vars)))
  (test-equal (term (eval-expr x ,my-vars)) 10))
