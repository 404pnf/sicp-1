#lang racket

#|

In this original definition of amb-choices,

(define (amb-choices expr) (rest expr))

instead return the shuffled values

(define (amb-choices expr) (shuffle (rest expr)))

|#