#lang rosette

; (require "lang/datastore.rkt")
(require redex)

(define-symbolic b boolean?)
(define-symbolic i integer?)

; Unfortunately it's not so straightforward to use Redex with Rosette
;
; (define (my-prog val)
;   (term ((ife x (0) (1)) () ((x (symbolic ,val))))))
; (apply-reduction-relation* red (my-prog b))
; (define sol
;   (solve (assert (= (term (0 () ((x, #t)))) (apply-reduction-relation* red (my-prog b))))))
;
; redex doesn't know how to reduce the variable value because of the symbolic value

; Have to manually define semantics :(

(define (binop? op) #f)

(define (eval-expr/fold exprs ctx vars)
  (for/fold ([vals '()] [c ctx] [v vars])
            ([e exprs])
    (match-let ([`(,valn ,cn ,vn) (eval-expr e c v)])
      (values (cons valn vals) cn vn))))

(define/match (eval-expr expr ctx vars)
  [((? number? lit) _ _) `(,lit ,ctx ,vars)]
  [((? boolean? lit) _ _) `(,lit ,ctx ,vars)]
  [((? symbol? x) _ _)
   `(,(cadr (assoc x vars)) ,ctx ,vars)]
  [(`(let ,(? symbol? x) ,expr) _ _)
   (match-let ([(list val nctx nvars) (eval-expr expr ctx vars)])
     `(,val ,nctx ((x ,val) ,@nvars)))]
  [(`(+ ,e ...) _ _)
   (let-values ([(vals nctx nvars) (eval-expr/fold e ctx vars)])
     `(,(apply + vals) ,nctx ,nvars))]
  [(`(block) _ _) `((void) ,ctx ,vars)]
  [(`(block ,e) _ _) (eval-expr e ctx vars)]
  [(`(block ,e ,en ...) _ _)
   (match-let ([(list val nctx nvars) (eval-expr e ctx vars)])
     (eval-expr `(block ,@en) nctx nvars))]
  [(_ _ _) 'stuck])

(define (run-program prog)
  (match-let ([(list val ctx vars) (eval-expr prog '() '())])
    val))

(solve (assert (= 10  (run-program `(block (let x ,i) (+ x 10))))))
