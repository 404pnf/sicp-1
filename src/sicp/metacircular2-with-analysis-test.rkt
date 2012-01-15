#lang racket

(require "metacircular2-with-analysis.rkt"
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
                   "let* test")
     (eval '(define (f x)
              (define (even? n)
                (if (= n 0)
                    true
                    (odd? (- n 1))))
              (define (odd? n)
                (if (= n 0)
                    false
                    (even? (- n 1))))
              (odd? x))
           env1)
     (check-equal? (eval '(f 2) env1) false "internal definitions")
     (check-equal? (eval '(f 3) env1) true "internal definitions")
     (eval '(define (f1 x)
              (letrec ((even?
                        (lambda (n)
                          (if (= n 0)
                              true
                              (odd? (- n 1)))))
                       (odd?
                        (lambda (n)
                          (if (= n 0)
                              false
                              (even? (- n 1))))))
                (even? x)))
           env1)
     (check-equal? (eval '(f1 2) env1) true "internal definitions")
     (check-equal? (eval '(f1 3) env1) false "internal definitions")
     (eval '(define (fib n)
              (let fib-iter ((a 1)
                             (b 0)
                             (count n))
                (if (= count 0)
                    b
                    (fib-iter (+ a b) a (- count 1)))))
           env1)
     (check-equal? (eval '(fib 10) env1) 55 "named let")
     (eval '(define (factorial n)
              (if (= n 1)
                  1
                  (* (factorial (- n 1)) n)))
           env1)
     (check-equal? (eval '(factorial 10) env1) 3628800 "factorial test"))))


(run-tests metacircular2-tests)