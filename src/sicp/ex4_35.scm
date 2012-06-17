;; done in Chicken scheme which has a builtin amb operator
;; - can be installed with
;;   $ chicken-install amb

(use amb)
(use srfi-1)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-between low high)
  (let [(count (- high low))]
    (let [(items (iota count low))]
      (an-element-of items))))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;; another implementation
(define (an-integer-between-2 low high)
  (require (< low high))
  (amb low (an-integer-between-2 (+ low 1) high)))

;; play with it
(a-pythagorean-triple-between 1 20)
