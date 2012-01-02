#lang racket

(provide let->combination let?)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (variable? expr)
  (symbol? expr))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (let? expr)
  (tagged-list? expr 'let))

(define (named-let? expr)
  (if (variable? (cadr expr))
      #t
      #f))

(define (let-name expr)
  (if (named-let? expr)
      (cadr expr)
      #f))

(define (let-bindings expr) 
  (if (let-name expr) 
      (caddr expr)
      (cadr expr)))

(define (let-bindings-variables bindings) (map car bindings))
(define (let-bindings-values bindings) (map cadr bindings))
(define (let-body expr) 
  (if (let-name expr)
      (cdddr expr)
      (cddr expr)))

(define (let->combination expr)
  (let ([bindings (let-bindings expr)])
    (let ([vars (let-bindings-variables bindings)]
          [vals (let-bindings-values bindings)]
          [body (let-body expr)])
      (if (not (let-name expr))
          (cons (make-lambda vars body) vals)
          (list 'begin 
                (list 'define (let-name expr) (make-lambda vars body))
                (list (let-name expr) vals))))))

(define ns (make-base-namespace))
(eval (let->combination '(let ((x 2) (y 3)) (+ x y))) ns)
(eval (let->combination '(let ((x 2) (y 3)) (+ x y))) ns)
(let->combination '(let fib-iter ((a 1)
                                       (b 0)
                                       (count n))
                          (if (= count 0)
                              b
                              (fib-iter (+ a b) a (- count 1)))))
