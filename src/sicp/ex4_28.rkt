#lang racket

(require "metacircular2-lazy.rkt")

#|
Forcing is needed for any higher order procedure. An example is map. If you evaluate the following 
code with `actual-value' instead of `eval' of operator, it executes fine but if not, then eval gets
a thunk object (the two operands) for evaluation and when it reaches `apply' inside the `cons' it 
will fail as `apply' does not know about thunk objects.
|#

(define env1 (make-environment))
(eval '(define (map f xs)
         (if (null? xs)
             '()
             (cons (f (car xs)) (map f (cdr xs)))))
      env1)
(eval '(map (lambda(x) (* x x)) '(1 2 3)) env1)