#lang racket

(require "metacircular2-lazy.rkt")

(define env1 (make-environment))
(eval '(define count 0) env1)
(eval '(define (id x) (set! count (+ count 1)) x) env1)

#|

> (eval '(define w (id (id 0))) env1)
> (eval 'count env1)
1
> (eval 'w env1)
#0='(thunk
     (id 0)
     #1=(#hash((< . (primitive #<procedure:<>))
               (* . (primitive #<procedure:*>))
               (> . (primitive #<procedure:>>))
               (- . (primitive #<procedure:->))
               (= . (primitive #<procedure:=>))
               (true . #t)
               (+ . (primitive #<procedure:+>))
               (list . (primitive #<procedure:list>))
               (null? . (primitive #<procedure:null?>))
               (/ . (primitive #<procedure:/>))
               (car . (primitive #<procedure:car>))
               (false . #f)
               (cdr . (primitive #<procedure:cdr>))
               (cons . (primitive #<procedure:cons>))
               (count . 1)
               (id . (procedure (x) ((set! count (+ count 1)) x) #1#))
               (w . #0#))))
> (eval 'count env1)
1
> 

|#