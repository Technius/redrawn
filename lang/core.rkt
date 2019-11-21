#lang rosette

(provide binop? get-op core-eval core-eval/fold run-program compose-interpreter)
(require racket/function)

(define (ops/or . vals) (ormap identity vals))
(define (ops/and . vals) (andmap identity vals))
(define ops
  '((+ +)
    (- -)
    (&& ops/and)
    (|| ops/or)
    (== equal?)))
(define-namespace-anchor ops-anchor)
(define (get-op op)
  (let ([ns (namespace-anchor->namespace ops-anchor)])
    (parameterize ([current-namespace ns])
      (match (assoc op ops)
        [#f #f]
        [`(,_ ,f) (eval f ns)]))))
(define (binop? op) (not (eq? (get-op op) #f)))

(define (core-eval/fold f exprs ctx vars)
  (for/fold ([vals '()] [c ctx] [v vars] #:result `(,(reverse vals) ,c ,v))
            ([e exprs])
    (match-let ([(list valn cn vn) (f e c v)])
      (values (cons valn vals) cn vn))))
; Interpreter for core language terms.
(define (core-eval f expr ctx vars)
  (match expr
    [(? number? lit) `(,lit ,ctx ,vars)]
    [(? boolean? lit) `(,lit ,ctx ,vars)]
    [(? symbol? x)
     (define result (assoc x vars))
     (assert (not (equal? #f result)) (format "variable not defined: ~a" x))
     `(,(cadr result) ,ctx ,vars)]
    [`(let ,(? symbol? x) ,expr)
     (match-let ([(list val nctx nvars) (f expr ctx vars)])
       `(,val ,nctx ((,x ,val) ,@nvars)))]
    [`(,(? binop? op) ,e ...)
     (match-let
         ([(list vals nctx nvars) (core-eval/fold f e ctx vars)]
          [opf (get-op op)])
       `(,(apply opf vals) ,nctx ,nvars))]
    [`(block) `((void) ,ctx ,vars)]
    [`(block ,e) (f e ctx vars)]
    [`(block ,e ,en ...)
     (match-let ([(list val nctx nvars) (f e ctx vars)])
       (f `(block ,@en) nctx nvars))]
    [`(if ,guard ,texpr ,fexpr)
     (match-let ([(list guardv nctx nvars) (f guard ctx vars)])
       (f (if guardv texpr fexpr) nctx nvars))]
    [_ (error 'evaluation "stuck on ~e" expr)]))

; Compose interpreters from right to left (i.e. i1 is executed before i2)
(define (compose-interpreter i1 i2)
  (lambda (f expr ctx vars)
    (define (cf e c v) (i2 f e c v))
    (i1 cf expr ctx vars)))

; Runs the given program with the given interpreter. An interpreter has the form
;     interpreter : (expr ctx vars -> expr) expr ctx vars -> expr
; i.e. an interpreter is a function that will evaluate the next step and then
; pass the result to the given continuation. By setting the continuation to the
; interpreter itself (i.e. a fixed point of the interpreter), we can evaluate
; arbitrary terms.
;
; Two interpreters can be composed to get another interpreter.
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
  (check-equal? (run '(- 50 10)) 40)
  (check-equal? (run '(block 10 20)) 20)
  (check-equal? (run '(if (|| #t #f) 10 20)) 10)
  (check-equal? (run '(if (|| #f #f) 10 20)) 20)
  (check-equal? (run '(block (let x 10) (+ x 10))) 20)
  (check-equal? (run '(block (if #t (let y 10) (let y 20)) y)) 10)
  (check-equal? (run '(block (if #f (let y 10) (let y 20)) y)) 20)
  )

; Numbers symbols in an expr AST, returning a tree containing the corresponding
; ids
(define (number-nodes expr)
  (define (go e cur-id)
    (if (list? e)
        (for/fold ([id cur-id] [node '()] #:result (values (reverse node) id))
                  ([en e])
          (let-values ([(nchild nid) (go en id)])
            (values nid (cons nchild node))))
        (values cur-id (+ cur-id 1))))
  (call-with-values (lambda () (go expr 0)) (lambda (n i) n)))
