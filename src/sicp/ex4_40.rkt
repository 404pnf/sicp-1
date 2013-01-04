#lang racket

#|

how many sets of assignments are there of people to floors, both before and after the requirement that floor assignments be distinct? 

Before - 5 * 5 * 5 * 5 * 5 = 3125
After  - 5 * 4 * 3 * 2 * 1 = 120

|#

(require "amb-eli.rkt")
(require "distinct.rkt")

(define (multiple-dwelling-1)
  (let ([baker (amb 1 2 3 4)]
        [cooper (amb 2 3 4 5)]
        [fletcher (amb 2 3 4)]
        [miller (amb 3 4 5)]
        [smith (amb 1 2 3 4 5)])
    ;(assert (not (= baker 5)))
    ;(assert (not (= cooper 1)))
    ;(assert (not (= fletcher 5)))
    ;(assert (not (= fletcher 1)))
    (assert (> miller cooper))
    (assert (not (= (abs (- smith fletcher)) 1)))
    (assert (not (= (abs (- fletcher cooper)) 1)))
    (assert (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(collect (multiple-dwelling-1))

;; another implementation
(define (multiple-dwelling-2)
  (let ([cooper (amb 2 3 4 5)]
        [miller (amb 3 4 5)])
    (assert (> miller cooper))
    (let ([fletcher (amb 2 3 4)])
      (assert (not (= (abs (- fletcher cooper)) 1)))
      (let ([smith (amb 1 2 3 4 5)])
        (assert (not (= (abs (- smith fletcher)) 1)))
        (let ([baker (amb 1 2 3 4)])
          (assert (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

(collect (multiple-dwelling-2))