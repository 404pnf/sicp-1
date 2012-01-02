#lang racket

(require "metacircular2.rkt"
         rackunit)

(require rackunit/text-ui)

(define metacircular2-tests
  (test-suite "test suite for meta circular evaluator"
   (let ([env1 (make-environment)])
     (check-equal? (eval '(+ 1 1) env1) 2 "simple addition")
     (check-equal? (eval '(- 2 1) env1) 1 "simple subtraction")
     (check-equal? (eval '(quote x) env1) 'x "quote")
     (eval '(define x 20) env1)
     (check-equal? (eval 'x env1) 20 "definition of identifiers with simple values")
     (eval '(set! x 42) env1) 
     (check-equal? (eval 'x env1) 42 "set!")
     (eval '(define (square x) (* x x)) env1)
     (check-equal? (eval '(square 10) env1) 100 "simple function definition")
     (eval '(define (square x) (let ([s (* x x)]) s)) env1)
     (check-equal? (eval '(square 20) env1) 400 "different way to define square")
     (eval '(define (absolute x)
              (cond ((> x 0) x)
                    ((= x 0) (display 'zero) 0)
                    (else (- x))))
           env1)
     (check-equal? (eval '(absolute -2) env1) 2 "conditionals")
     (check-equal? (eval '(absolute 2) env1) 2 "conditionals")
     (eval '(define (foo) (let ((x 42) (y 100)) (list x y))) env1)
     (check-equal? (eval '(foo) env1) '(42 100) "simple let")
     (check-equal? (eval '(let* ((x 3)
                                 (y (+ x 2))
                                 (z (+ x y 5)))
                            (* x z))
                         env1)
                   39
                   "let* test"))))


(run-tests metacircular2-tests)