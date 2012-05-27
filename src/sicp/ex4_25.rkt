#lang racket

#|

The call (factorial 5) will never call 'unless' and will be recursively call 'factorial' for ever. 'n' goes
negative and the factorial won't notice it, as it will never get to evaluate the function body of `unless'.

It will work in a normal order language because none of the arguments of 'unless' are evaluated until
they are used inside.

#lang lazy
(define (unless predicate if-exp else-exp)
  (if (not predicate)
      if-exp
      else-exp))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

|#