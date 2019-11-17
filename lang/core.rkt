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
        (void)
        (let x expr)
        (binop expr expr ...)
        (block expr ...)
        (ife expr (expr ...) (expr ...)))
  (binop + and or eq?)
  (x variable-not-otherwise-mentioned))

;; Core language evaluation contexts
(define-extended-language
  CoreE
  Core
  (Ee hole
      (block Ee expr ...)
      (binop v ... Ee expr ...)
      (ife Ee (expr ...) (expr ...))
      (let x Ee))
  (vars ((x v) ...)) ;; variables
  (ctx (void)) ;; language-specific context
  (v number string boolean (void)))

(define-metafunction
  CoreE
  extend-vars : vars x v -> vars
  [(extend-vars ((x_1 v_1) ... (x v_e) (x_2 v_2) ...) x v) ((x_1 v_1) ... (x v) (x_2 v_2) ...)]
  [(extend-vars vars x v) ,(cons (term (x  v)) (term vars))])

(define-metafunction
  CoreE
  load-var : vars x -> v
  [(load-var ((x_1 v_1) ... (x v) (x_2 v_2) ...) x) v]
  [(load-var any_1 any_2) ,(error 'load-var "not found: ~e" (term x))])

;; Core expression small-step operational semantics
(define red
  (reduction-relation
   CoreE
   #:domain (expr ctx vars)
   ; Variable loads
   (--> [(in-hole Ee x) ctx vars]
        [(in-hole Ee (load-var vars x)) ctx vars]
        "var")
   ; Binary operations
   (--> [(in-hole Ee (+ number ...)) ctx vars]
        [(in-hole Ee ,(apply + (term (number ...)))) ctx vars]
        "+")
   (--> [(in-hole Ee (or boolean ...)) ctx vars]
        [(in-hole Ee ,(ormap identity (term (boolean ...)))) ctx vars]
        "or")
   (--> [(in-hole Ee (and boolean ...)) ctx vars]
        [(in-hole Ee ,(andmap identity (term (boolean ...)))) ctx vars]
        "and")
   (--> [(in-hole Ee (eq? v_1 v_2)) ctx vars]
        [(in-hole Ee ,(apply eq? (term (v_1 v_2)))) ctx vars]
        "eq")
   ; Let
   (--> [(in-hole Ee (let x v)) ctx vars]
        [(in-hole Ee v) ctx (extend-vars vars x v)]
        "let")
   ; If expressions
   (--> [(in-hole Ee (ife #t (expr_t ...) (expr_f ...))) ctx vars]
        [(in-hole Ee (block expr_t ...)) ctx vars]
        "if_t")
   (--> [(in-hole Ee (ife #f (expr_t ...) (expr_f ...))) ctx vars]
        [(in-hole Ee (block expr_f ...)) ctx vars]
        "if_f")
   ; Blocks
   (--> [(in-hole Ee (block v)) ctx vars]
        [(in-hole Ee v) ctx vars]
        "block_v")
   (--> [(in-hole Ee (block v expr expr_n ...)) ctx vars]
        [(in-hole Ee (block expr expr_n ...)) ctx vars]
        "block_n")
   ))

(define-metafunction
  CoreE
  eval-expr : expr ctx vars -> v or stuck
  [(eval-expr v ctx vars) v]
  [(eval-expr expr ctx vars)
   (eval-expr expr_next ctx_next vars_next)
   (where ((expr_next ctx_next vars_next)) ,(apply-reduction-relation red (term (expr ctx vars))))]
  [(eval-expr any_1 any_2 any_3) stuck])

(module+ test
  (define my-vars (term ((x 10) (y 20) (z 30))))
  (test-->> red
            (term (x (void) ,my-vars))
            (term (10 (void) ,my-vars)))
  (test-->> red
            (term ((+ (+ 30 40) (+ 10 20)) (void) ()))
            (term (100 (void) ())))
  (test-->> red
            (term ((or #f #f #t) (void) ()))
            (term (#t (void) ())))
  (test-->> red
            (term ((and #f #f #t) (void) ()))
            (term (#f (void) ())))
  (test-->> red
            (term ((eq? 1 1) (void) ()))
            (term (#t (void) ())))
  (test-->> red
            (term ((ife #t (1) (0)) (void) ()))
            (term (1 (void) ())))
  (test-->> red
            (term ((ife #f (1) (0)) (void) ()))
            (term (0 (void) ())))
  (test-->> red
            (term ((ife (eq? x y) (x) ((+ x y))) (void) ,my-vars))
            (term (30 (void) ,my-vars)))
  (test-equal (term (eval-expr x (void) ,my-vars)) 10))
