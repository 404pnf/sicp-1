#lang racket

(require "amb-eli.rkt")

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between low high)
  (assert (< low high))
  (amb low (an-integer-between (+ low 1) high)))

#|

If we replace an-integer-between with an-integer-starting-from, then
the k grows too high to invalid ranges and never stops. The way to accomplish
correct values is to restrict i, j and k to valid ranges.

|#

;; using euclid's formula
;; http://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
(define (pythagorean-triples)
  (let [(n (an-integer-starting-from 1))]
    (let [(m (an-integer-starting-from n))]
      (assert (> m n))
      (list (- (sqr m) (sqr n))
            (* 2 m n)
            (+ (sqr m) (sqr n))))))