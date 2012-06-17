#lang racket

(require "amb-eli.rkt")

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (assert (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (assert (integer? k))
          (list i j k))))))

#|

Yes, Ben is correct. The above program prunes the search space by restricting the
possible values of k.

k^2 <= high^2

and sqrt (i^2 + j^2) is an integer. This eliminates a large number of (i, j, k) triples
and hence the search space is a lot less than the naive implementation in the text.

|#
