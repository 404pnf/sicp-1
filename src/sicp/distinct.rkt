#lang racket

(provide distinct?)

(define (distinct? xs)
  (let loop [(s (set))
             (r xs)]
    (cond [(empty? r) #t]
          [(set-member? s (first r)) #f]
          [else (loop (set-add s (first r)) (rest r))])))

(module+ test
  (require rackunit)
  
  (check-equal? (distinct? '()) #t)
  (check-equal? (distinct? '(1)) #t)
  (check-equal? (distinct? '(1 2)) #t)
  (check-equal? (distinct? '(1 1)) #f)
  (check-equal? (distinct? '(1 2 3)) #t)
  (check-equal? (distinct? '(1 2 3 3 2)) #f)
  (check-equal? (distinct? '(a b)) #t)
  (check-equal? (distinct? '(a b a)) #f)
  (check-equal? (distinct? '(a b c c)) #f)
  (check-equal? (distinct? '(1 2 3 4)) #t)
  (check-equal? (distinct? '(1 2 3 4 5)) #t)
  (check-equal? (distinct? '(1 (2 3) 4 2 3)) #t))