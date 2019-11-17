#lang rosette

(provide binop? get-op eval-expr run-program)
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
  [(`(,(? binop? op) ,e ...) _ _)
   (let-values
       ([(vals nctx nvars) (eval-expr/fold e ctx vars)]
        [(opf) (get-op op)])
     `(,(apply opf vals) ,nctx ,nvars))]
  [(`(block) _ _) `((void) ,ctx ,vars)]
  [(`(block ,e) _ _) (eval-expr e ctx vars)]
  [(`(block ,e ,en ...) _ _)
   (match-let ([(list val nctx nvars) (eval-expr e ctx vars)])
     (eval-expr `(block ,@en) nctx nvars))]
  [(`(if ,guard ,texpr ,fexpr) _ _)
   (match-let ([(list guardv nctx nvars) (eval-expr guard ctx vars)])
     (eval-expr (if guardv texpr fexpr) nctx nvars))]
  [(_ _ _) 'stuck])

(define (run-program prog)
  (match-let ([(list val ctx vars) (eval-expr prog '() '())])
    val))

(module+ test
  (require rackunit)
  (check-equal? (run-program 10) 10)
  (check-equal? (run-program '(+ 10 20 30)) 60)
  (check-equal? (run-program '(block 10 20)) 20)
  (check-equal? (run-program '(if (|| #t #f) 10 20)) 10)
  (check-equal? (run-program '(if (|| #f #f) 10 20)) 20)
  )
