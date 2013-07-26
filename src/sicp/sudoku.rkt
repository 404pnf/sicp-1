#lang racket

(require "amb-eli.rkt"
         "distinct.rkt")

(define (positions size)
  (for*/list ([x (in-range 1 (+ size 1))]
              [y (in-range 1 (+ size 1))])
    (list x y)))

;; board is represented as list of lists
;;

(define (sudoku board)
  (let ([size (length board)])
    (let ([new-board (amb-board board)])
      (for ([r new-board])
        (assert (distinct? r)))
      (for ([c (apply map list new-board)])
        (assert (distinct? c)))
      (let ([ss (segments size)])
        (for ([s ss])
          (assert (distinct? s)))
        new-board))))


(define (amb-board board)
  (cond [(empty? board) empty]
        [else 
         (let ([row (first board)])
           (cons (for/list ([r row])
                   (if (eq? r '?)
                       (amb-list (set->list (set-subtract (set 1 2 3 4 5 6 7 8 9) (list->set row))))
                       r))
                 (amb-board (rest board))))]))

(define (segments board)
  (let ([size (length board)])
    (let ([s (sqrt size)])
      (for*/list ([i (in-range 0 size s)]
                  [j (in-range 0 size s)])
        (for*/list ([x (in-range i (+ i s))]
                    [y (in-range j (+ j s))])
          (list-ref (list-ref board x) y))))))



#|

((? ? 8 ? ? ? 1 5 ?)
 (? ? ? ? ? 1 8 ? ?)
 (3 ? 5 4 ? ? ? ? 9)
 (5 ? ? ? ? 9 ? ? ?)
 (? 9 ? 2 3 4 ? 7 ?)
 (? ? ? 1 ? ? ? ? 8)
 (4 ? ? ? ? 5 9 ? 1)
 (? ? 6 7 ? ? ? ? ?)
 (? 5 3 ? ? ? 2 ? ?))

|#

