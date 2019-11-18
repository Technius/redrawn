#lang rosette

(provide binop? get-op core-eval core-eval/fold run-program)
(require racket/function)

(define (ops/or . vals) (ormap identity vals))
(define (ops/and . vals) (andmap identity vals))
(define ops
  '((+ +)
    (- -)
    (&& ops/and)
    (|| ops/or)
    (== equals?)))
(define-namespace-anchor ops-anchor)
(define (get-op op)
  (let ([ns (namespace-anchor->namespace ops-anchor)])
    (parameterize ([current-namespace ns])
      (match (assoc op ops)
        [#f #f]
        [`(,_ ,f) (eval f ns)]))))
(define (binop? op) (not (eq? (get-op op) #f)))

(define (core-eval/fold f exprs ctx vars)
  (for/fold ([vals '()] [c ctx] [v vars])
            ([e exprs])
    (match-let ([`(,valn ,cn ,vn) (f e c v)])
      (values (cons valn vals) cn vn))))

; Fixed point evaluation.
; This is extensible
(define (core-eval f expr ctx vars)
  (match expr
    [(? number? lit) `(,lit ,ctx ,vars)]
    [(? boolean? lit) `(,lit ,ctx ,vars)]
    [(? symbol? x)
     `(,(cadr (assoc x vars)) ,ctx ,vars)]
    [`(let ,(? symbol? x) ,expr)
     (match-let ([(list val nctx nvars) (f expr ctx vars)])
       `(,val ,nctx ((x ,val) ,@nvars)))]
    [`(,(? binop? op) ,e ...)
     (let-values
         ([(vals nctx nvars) (core-eval/fold f e ctx vars)]
          [(opf) (get-op op)])
       `(,(apply opf vals) ,nctx ,nvars))]
    [`(block) `((void) ,ctx ,vars)]
    [`(block ,e) (f e ctx vars)]
    [`(block ,e ,en ...)
     (match-let ([(list val nctx nvars) (f e ctx vars)])
       (f `(block ,@en) nctx nvars))]
    [`(if ,guard ,texpr ,fexpr)
     (match-let ([(list guardv nctx nvars) (f guard ctx vars)])
       (f (if guardv texpr fexpr) nctx nvars))]
    [_ 'stuck]))

(define (run-program interpreter prog)
  (define (eval-fun expr ctx vars)
    (interpreter eval-fun expr ctx vars))
  (match-let ([(list val ctx vars) (eval-fun prog '() '())])
    val))

(module+ test
  (require rackunit)
  (define (run prog) (run-program core-eval prog))
  (check-equal? (run 10) 10)
  (check-equal? (run '(+ 10 20 30)) 60)
  (check-equal? (run '(block 10 20)) 20)
  (check-equal? (run '(if (|| #t #f) 10 20)) 10)
  (check-equal? (run '(if (|| #f #f) 10 20)) 20)
  )
