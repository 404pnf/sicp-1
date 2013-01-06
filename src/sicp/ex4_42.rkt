#lang racket

(require "amb-eli.rkt")
(require "distinct.rkt")

(define (xor x y)
  (cond [(and x y) #f]
        [(and (not x) (not y)) #f]
        [else #t]))

(define (liars)
  (let ([betty (amb 1 2 3 4 5)]
        [ethel (amb 1 2 3 4 5)]
        [joan  (amb 1 2 3 4 5)]
        [kitty (amb 1 2 3 4 5)]
        [mary  (amb 1 2 3 4 5)])
    (assert (distinct? (list betty ethel joan kitty mary)))
    (assert (xor (= kitty 2) (= betty 3)))
    (assert (xor (= ethel 1) (= joan 2)))
    (assert (xor (= joan 3)  (= ethel 5)))
    (assert (xor (= kitty 2) (= mary 4)))
    (assert (xor (= mary 4)  (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

(collect (liars))

;; '(((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4)))
