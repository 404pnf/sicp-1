#lang racket
(require "distinct.rkt")

(define (multiple-dwelling)
  (for/list ([baker '(1 2 3 4 5)]
             #:unless (= baker 5)
             [cooper '(1 2 3 4 5)]
             #:unless (= cooper 1)
             [fletcher '(1 2 3 4 5)]
             #:unless (or (= fletcher 1)
                          (= fletcher 5))
             [miller '(3 4 5)]
             #:when (> miller cooper)
             [smith '(1 2 3 4 5)]
             #:when (and (not (= (abs (- smith fletcher)) 1))
                         (not (= (abs (- fletcher cooper)) 1))
                         (distinct? (list baker cooper fletcher miller smith))))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)