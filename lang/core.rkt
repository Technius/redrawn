#lang rosette

(provide binop? get-op
         rosette-eval core-eval core-eval/fold
         compose-interpreter compose-interpreter*
         run-program number-nodes)
(require racket/function)

(define (ops/or . vals) (ormap identity vals))
(define (ops/and . vals) (andmap identity vals))
(define ops
  ; Language symbol, racket function, type
  '((+ + integer?)
    (- - integer?)
    (&& ops/and boolean?)
    (|| ops/or boolean?)
    (== equal? (const #t))))
(define-namespace-anchor ops-anchor)
(define (get-op op)
  (let ([ns (namespace-anchor->namespace ops-anchor)])
    (parameterize ([current-namespace ns])
      (match (assoc op ops)
        [#f #f]
        [(list _ f ty) (list (eval f ns) (eval ty ns))]))))
(define (binop? op) (not (eq? (get-op op) #f)))

(define (core-eval/fold f exprs ctx vars)
  (for/fold ([vals '()] [c ctx] [v vars] #:result `(,(reverse vals) ,c ,v))
            ([e exprs])
    (match-let ([(list valn cn vn) (f e c v)])
      (values (cons valn vals) cn vn))))

; Stuck
(define (stuck f expr ctx vars)
  (error 'evaluation "stuck on ~e" expr))

; Interpreter for core language terms.
(define (core-eval cont f expr ctx vars)
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
     (match-define (list vals nctx nvars) (core-eval/fold f e ctx vars))
     (match-define (list opf ty) (get-op op))
     (when (not (for/and ([v vals]) (ty v)))
       (error 'core "Type error: expected only ~a, got arguments ~e" ty vals))
     `(,(apply opf vals) ,nctx ,nvars)]
    [`(block) `((void) ,ctx ,vars)]
    [`(block ,e) (f e ctx vars)]
    [`(block ,e ,en ...)
     (match-let ([(list val nctx nvars) (f e ctx vars)])
       (f `(block ,@en) nctx nvars))]
    [`(if ,guard ,texpr ,fexpr)
     (match-define (list guardv nctx nvars) (f guard ctx vars))
     (when (not (boolean? guardv))
       (error 'core "Type error: expected boolean?, got ~e" guardv))
     (f (if guardv texpr fexpr) nctx nvars)]
    [_ (cont f expr ctx vars)]))

; Handler for Rosette terms
(define (rosette-eval cont f expr ctx vars)
  (match expr
    [(? term? lit) `(,lit ,ctx ,vars)]
    [(? union? su)
;     (println "In union (1)")
     (for/all ([se su])
       ;       (begin (printf "Union(1): ~A\n" se) (f se ctx vars)))]
       (f se ctx vars))]
    [`(,(? union? su) ,e ...)
;     (println "In union (2)")
     (for/all ([se su])
;       (begin (printf "Union(2): ~A\n" se) (f `(,se ,@e) ctx vars)))]
       (f `(,se ,@e) ctx vars))]
    [_ (cont f expr ctx vars)]))

; Compose interpreters from right to left (i.e. i1 is executed before i2)
(define (compose-interpreter i1 i2)
  (lambda (cont f expr ctx vars)
    (define (cf f e c v) (i2 cont f e c v))
    (i1 cf f expr ctx vars)))

; compose-interpreter but for multiple interpreters
(define (compose-interpreter* . is)
  (for/fold ([interp (car is)])
            ([i (cdr is)])
    (compose-interpreter interp i)))

; Runs the given program with the given interpreter. An interpreter has the form
;     interpreter : (expr ctx vars -> expr) expr ctx vars -> expr
; i.e. an interpreter is a function that will evaluate the next step and then
; pass the result to the given continuation. By setting the continuation to the
; interpreter itself (i.e. a fixed point of the interpreter), we can evaluate
; arbitrary terms.
;
; Two interpreters can be composed to get another interpreter.
(define (run-program interpreter prog #:init-vars [init-vars '()])
  (define (eval-fun expr ctx vars)
;    (printf "Step: ~A\n" expr)
    (interpreter stuck eval-fun expr ctx vars))
  (match-let ([(list val ctx vars) (eval-fun prog '() init-vars)])
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
