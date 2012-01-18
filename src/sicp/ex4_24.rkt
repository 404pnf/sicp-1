#lang racket

;;; without analysis

#|

> (t '(begin 
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (factorial 1000)))
27.14208984375
> (t '(begin 
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (factorial 1000)))
27.64404296875
> (t '(begin 
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (factorial 10000)))
417.66796875
> (t '(begin 
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (factorial 10000)))
416.1162109375
> (t '(begin 
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (factorial 10000)))
439.837890625
> 

|#

;; with analysis

#|

>  (t '(begin 
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (factorial 1000)))
15.368896484375
>  (t '(begin 
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (factorial 1000)))
15.39111328125
>  (t '(begin 
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (factorial 10000)))
298.23779296875
>  (t '(begin 
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (factorial 10000)))
252.77978515625
>  (t '(begin 
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (factorial 10000)))
255.87890625
> 

|#