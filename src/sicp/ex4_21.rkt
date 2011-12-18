#lang racket

(define Y (lambda (f)
            ((lambda (g) 
               (g g))
             (lambda (h)
               (lambda (x)
                 ((f (h h)) x))))))

;; factorial
((Y (lambda (fct) 
      (lambda (n) 
        (if (= n 1) 
            1 
            (* n (fct (- n 1))))))) 
 10)

;; fibonacci
((Y (lambda (fib) 
      (lambda (n) 
        (cond 
          ((= n 0) 0) 
          ((= n 1) 1) 
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))))
 10)

;; b
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(eq? (f 3) #f)
(eq? (f 4) #t)