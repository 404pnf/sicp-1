#lang racket

(require "amb-eli.rkt")
(require "distinct.rkt")

(define (multiple-dwelling)
  (let [(baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5))]
    (assert (distinct? (list baker cooper fletcher miller smith)))
    (assert (not (= baker 5)))
    (assert (not (= cooper 1)))
    (assert (not (= fletcher 5)))
    (assert (not (= fletcher 1)))
    (assert (> miller cooper))
    (assert (not (= (abs (- smith fletcher)) 1)))
    (assert (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)