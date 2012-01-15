#lang racket

(require (rename-in racket/base
                    (apply apply-in-underlying-scheme)
                    (eval eval-in-underlying-scheme)))

(provide (all-defined-out))

(define (self-evaluating? expr)
  (match expr
    [(? number? expr) #t]
    [(? char? expr) #t]
    [(? string? expr) #t]
    [_ #f]))

(define (variable? expr) (symbol? expr))


;; sequence
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; begin
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;; lambda
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
        (loop (car procs) (cdr procs)))))

;; environment data structures
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (let ([ht (make-hash)])
    (for-each (lambda (var val)
                (hash-set! ht var val))
              variables
              values)
    ht))

(define (frame-variables frame)
  (hash-keys frame))

(define (frame-values frame)
  (hash-values frame))

(define (add-binding-to-frame! var val frame)
  (hash-set! frame var val))

;; environment is a list of frames, most recent being the last 
;; one consed into the list
(define (extend-environment vars vals base-env)
  (let ([frame (make-frame vars vals)])
    (cons frame base-env)))

(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "unbound variable: " var)
      (let ([frame (first-frame env)])
        (let ([value (hash-ref frame var (lambda () (lookup-variable-value var (enclosing-environment env))))])
          (if (eq? value '*unassigned*)
              (error "evaluating a variable that is not assigned a value -- " var)
              value)))))

(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
      (error "unbound variable: " var)
      (let ([frame (first-frame env)])
        (if (hash-has-key? frame var)
            (hash-set! frame var val)
            (set-variable-value! var val (enclosing-environment env))))))

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (if (hash-has-key? frame var)
        (hash-set! frame var val)
        (add-binding-to-frame! var val frame))))

;; primitive procedure
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;; global env
(define (make-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (make-environment))

;; application
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; compound procedure
(define (make-procedure params body env)
  (list 'procedure params (scan-out-definitions body) env))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (scan-out-definitions (caddr p)))
(define (procedure-environment p) (cadddr p))

(define (execute-application procedure arguments)
  (match procedure
    [`(primitive ,f ...)  (apply-primitive-procedure procedure arguments)]
    [`(procedure ,f ...)  ((procedure-body procedure)
                           (extend-environment
                            (procedure-parameters procedure)
                            arguments
                            (procedure-environment procedure)))]
    [_                (error "Unknown procedure type -- APPLY" procedure)]))

;; truth
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; cond
(define (cond->if clauses)
  (define (seq->exp actions) 
    (if (empty? (cdr actions)) 
        (car actions) 
        `(begin ,@actions)))
  (if (empty? clauses)
      'false
      (let ([clause (car clauses)])
        (match clause
          [`(else ,action ...) (seq->exp action)]
          [`(,pred ,action ...) `(if ,pred 
                                     ,(seq->exp action) 
                                     ,(cond->if (cdr clauses)))]))))

;; let
(define (let->combination lexpr)
  (match-let* ([`(let ,(? (lambda (x) (or (pair? x) (empty? x))) bindings) ,body ..1) lexpr]
               [`((,var ,val) ...) bindings])
    `((lambda ,var ,@body) ,@val)))

;; named let
(define (named-let->combination lexpr)
  (match-let* ([`(let ,(? symbol? name) ,(? (lambda (x) (or (pair? x) (empty? x))) bindings) ,body ..1) lexpr]
               [`((,var ,val) ...) bindings])
    `(begin (define ,name (lambda ,var ,@body))
            (,name ,@val))))

;; let*
(define (let*->nested-lets lexpr)
  (match lexpr
    [`(let* (,first-binding ,rest-bindings ...) ,body ..1)
     `(let (,first-binding) ,(let*->nested-lets `(let* ,rest-bindings ,@body)))]
     [`(let* () ,body ..1) `(let () ,@body)]))

;; internal definitions
(define (scan-out-definitions body)
  (match body
    [`((define ,var ,e) ..1 ,rest)
     `((let ,(map (lambda (v) (list (car v) ''*unassigned*)) var)
        ,@(map (lambda (v e) `(set! ,(car v) (lambda ,(cdr v) ,e))) var e)
        ,rest))]
    [_  body]))

;; letrec
(define (letrec->combination lexpr)
  (match lexpr
    [`(letrec (,bindings ...) ,body ..1)
     `(let ,(map (lambda (v) (list (car v) ''*unassigned*)) bindings)
        ,@(map (lambda (binding) 
                 (let ([name (car binding)]
                       [value (cadr binding)])
                   `(set! ,name ,value))) 
               bindings)
        ,@body)]))

(define (analyze exp)
  ;; use this display statement to visualize the recursive evaluation process
  ;;(display (format "~s~%" exp))
  (match exp
    [(? self-evaluating? exp) (lambda (env) exp)]
    [(? variable? exp) (lambda (env) (lookup-variable-value exp env))]
    [`(quote ,x) (lambda (env) x)]
    [`(set! ,var ,val) (let ([vproc (analyze val)])
                         (lambda (env) 
                           (set-variable-value! var (vproc env) env)))]
    [`(define ,(? (lambda (x) (not (pair? x))) var) ,b) (let ([vproc (analyze b)])
                                                          (lambda (env)
                                                            (define-variable! var (vproc env) env)))]
    [`(define ,(? pair? var) ,b ..1) (let ([pproc (analyze (make-lambda (cdr var) b))]
                                           [pname (car var)])
                                       (lambda (env)
                                         (define-variable! pname (pproc env) env)))]
    [`(if ,pred ,consequent ,alternative) (let ([pproc (analyze pred)]
                                                [cproc (analyze consequent)]
                                                [aproc (analyze alternative)])
                                            (lambda (env)
                                              (if (true? (pproc env))
                                                  (cproc env)
                                                  (aproc env))))]
    [`(lambda ,parameters ,body ..1) (let ([bproc (analyze-sequence body)])
                                       (lambda (env)
                                         (make-procedure parameters bproc env)))]
    [`(begin ,exp ...) (analyze-sequence exp)]
    [`(cond ,clauses ...) (analyze (cond->if clauses))]
    [`(let ,(? (lambda (x) (or (pair? x) (empty? x))) bindings) ,body ..1) (analyze (let->combination exp))]
    [`(let ,(? symbol? name) ,bindings ,body ..1) (analyze (named-let->combination exp))]
    [`(let* ,bindings ,body ..1) (analyze (let*->nested-lets exp))]
    [`(letrec ,bindings ,body ..1) (analyze (letrec->combination exp))]
    [(list f x ...) (let ([fproc (analyze f)]
                          [aprocs (map analyze x)])
                      (lambda (env)
                        (execute-application (fproc env)
                                             (map (lambda (proc)
                                                    (proc env))
                                                  aprocs))))]
    [_ (error "unable to evaluate expression -- EVAL " exp)]))

;; eval
(define (eval exp env)
  ((analyze exp) env))

(define (interpret)
  (let loop ([input (read)]
             [env the-global-environment])
    (let ([output (eval input env)])
      (display output)
      (loop (read) env))))

(define (t expr)
  (let ([t1 (current-inexact-milliseconds)])
    (eval expr the-global-environment)
    (displayln (- (current-inexact-milliseconds) t1))))