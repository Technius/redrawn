#lang racket

(require redex)
(require (prefix-in core: "core.rkt"))
(provide Datastore red ds-bs)

(define-extended-language
  Datastore
  core:CoreE
  (expr ....
        (store expr expr)
        (del expr))
  (Ee ....
     (store Ee expr)
     (store v Ee)
     (del Ee))
  ; Data store
  (ctx ((v v) ...)))

(define-metafunction
  Datastore
  extend-store : ctx v_k v_v -> ctx
  [(extend-store ((v_k1 v_v1) ... (v_k v) (v_k2 v_v2) ...) v_k v_v)
   ((v_k1 v_v1) ... (v_k v_v) (v_k2 v_v2) ...)]
  [(extend-store ctx v_k v_v) ,(cons (term (v_k  v_v)) (term ctx))])

; (define-metafunction/extension
;   core:eval-expr
;   Datastore
;   eval-expr : expr ctx vars -> v or stuck)

(define-extended-judgment-form
  Datastore
  core:bs
  #:mode (ds-bs I O)
  #:contract (ds-bs (expr ctx vars) (v ctx vars))
  )

(define red
  (extend-reduction-relation
   core:red
   Datastore
   (--> [(in-hole Ee (store v_1 v_2)) ctx vars]
        [(in-hole Ee (void)) (extend-store ctx v_1 v_2) vars]
        "store")
   ))

(module+ test
  (define store-prog
    (term ((block (store "foo" 20) (store "bar" 10))
           () ())))
  (test-->> red
            store-prog
            (term ((void) (("bar" 10) ("foo" 20)) ())))
)
