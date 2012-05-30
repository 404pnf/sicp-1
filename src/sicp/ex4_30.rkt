#lang racket

(require "metacircular2-lazy.rkt")

(define env1 (make-environment))
(eval '(define (for-each proc items)
         (if (null? items)
             'done
             (begin (proc (car items)) 
                    (for-each proc (cdr items)))))
      env1)
(eval '(for-each (lambda(x) (newline) (display x)) '(1 2 3)) env1)

#|

Part a.

One can observe the execution of this routine by a small modification to the 2 procedures that constitute
the lazy evaluator as follows (the only change is printing of the arguments)

(define (actual-value exp env)
  (display (format "eval expr ~s~%" exp))
  (newline)
  (force-it (eval exp env)))

(define (force-it obj)
  ;; (display (format "~s~%" obj))
  (match obj
    [`(thunk ,exp ,env) (begin
                          (display (format "eval's output ~s~%" obj))
                          (newline)
                          (actual-value exp env))]
    [_ obj]))

As we can observe during the execution, the `begin' gets transformed to `eval-sequence'
and then `newline' and `(display x)' are called. Both these are primitive expressions
and so the arguments are forced by apply.

The trace of the execution of the above definition and the evaluation are listed below.
|#

#|

eval expr for-each

eval expr (null? items)

eval expr null?

eval expr items

eval's output (thunk (quote (1 2 3)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))

eval expr (quote (1 2 3))

eval expr proc

eval's output (thunk (lambda (x) (newline) (display x)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))

eval expr (lambda (x) (newline) (display x))

eval expr newline


eval expr display

eval expr x

eval's output (thunk #0=(car items) (#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc #0#) (for-each proc (cdr items))))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))

eval expr (car items)

eval expr car

eval expr items

eval's output (thunk (quote (1 2 3)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))

eval expr (quote (1 2 3))

1eval expr for-each

eval expr (null? items)

eval expr null?

eval expr items

eval's output (thunk #0=(cdr items) (#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc #0#)))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))

eval expr (cdr items)

eval expr cdr

eval expr items

eval's output (thunk (quote (1 2 3)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))

eval expr (quote (1 2 3))

eval expr proc

eval's output (thunk proc (#hash((proc . (thunk (lambda (x) (newline) (display x)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))) (items . (thunk (quote (1 2 3)) #0#))) . #0#))

eval expr proc

eval's output (thunk (lambda (x) (newline) (display x)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))

eval expr (lambda (x) (newline) (display x))

eval expr newline


eval expr display

eval expr x

eval's output (thunk #0=(car items) (#hash((proc . (thunk proc #3=(#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc #0#) (for-each proc #2=(cdr items))))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))) (items . (thunk #2# #3#))) . #1#))

eval expr (car items)

eval expr car

eval expr items

eval's output (thunk #0=(cdr items) (#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc #0#)))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))

eval expr (cdr items)

eval expr cdr

eval expr items

eval's output (thunk (quote (1 2 3)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))

eval expr (quote (1 2 3))

2eval expr for-each

eval expr (null? items)

eval expr null?

eval expr items

eval's output (thunk #0=(cdr items) (#hash((proc . (thunk proc #2=(#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc #0#)))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))) (items . (thunk #0# #2#))) . #1#))

eval expr (cdr items)

eval expr cdr

eval expr items

eval's output (thunk #0=(cdr items) (#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc #0#)))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))

eval expr (cdr items)

eval expr cdr

eval expr items

eval's output (thunk (quote (1 2 3)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))

eval expr (quote (1 2 3))

eval expr proc

eval's output (thunk proc (#hash((proc . (thunk proc #2=(#hash((proc . (thunk (lambda (x) (newline) (display x)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc #1=(cdr items))))) #0#)))))) (items . (thunk (quote (1 2 3)) #0#))) . #0#))) (items . (thunk #1# #2#))) . #0#))

eval expr proc

eval's output (thunk proc (#hash((proc . (thunk (lambda (x) (newline) (display x)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))) (items . (thunk (quote (1 2 3)) #0#))) . #0#))

eval expr proc

eval's output (thunk (lambda (x) (newline) (display x)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))

eval expr (lambda (x) (newline) (display x))

eval expr newline


eval expr display

eval expr x

eval's output (thunk #0=(car items) (#hash((proc . (thunk proc #4=(#hash((proc . (thunk proc #3=(#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc #0#) (for-each proc #2=(cdr items))))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))) (items . (thunk #2# #3#))) . #1#))) (items . (thunk #2# #4#))) . #1#))

eval expr (car items)

eval expr car

eval expr items

eval's output (thunk #0=(cdr items) (#hash((proc . (thunk proc #2=(#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc #0#)))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))) (items . (thunk #0# #2#))) . #1#))

eval expr (cdr items)

eval expr cdr

eval expr items

eval's output (thunk #0=(cdr items) (#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc #0#)))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))

eval expr (cdr items)

eval expr cdr

eval expr items

eval's output (thunk (quote (1 2 3)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))

eval expr (quote (1 2 3))

3eval expr for-each

eval expr (null? items)

eval expr null?

eval expr items

eval's output (thunk #0=(cdr items) (#hash((proc . (thunk proc #3=(#hash((proc . (thunk proc #2=(#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc #0#)))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))) (items . (thunk #0# #2#))) . #1#))) (items . (thunk #0# #3#))) . #1#))

eval expr (cdr items)

eval expr cdr

eval expr items

eval's output (thunk #0=(cdr items) (#hash((proc . (thunk proc #2=(#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc #0#)))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))) (items . (thunk #0# #2#))) . #1#))

eval expr (cdr items)

eval expr cdr

eval expr items

eval's output (thunk #0=(cdr items) (#hash((proc . (thunk (lambda (x) (newline) (display x)) #1=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc #0#)))) #1#)))))) (items . (thunk (quote (1 2 3)) #1#))) . #1#))

eval expr (cdr items)

eval expr cdr

eval expr items

eval's output (thunk (quote (1 2 3)) #0=(#hash((newline . (primitive #<procedure:newline>)) (false . #f) (cons . (primitive #<procedure:cons>)) (cdr . (primitive #<procedure:cdr>)) (display . (primitive #<procedure:display>)) (car . (primitive #<procedure:car>)) (/ . (primitive #<procedure:/>)) (null? . (primitive #<procedure:null?>)) (list . (primitive #<procedure:list>)) (+ . (primitive #<procedure:+>)) (true . #t) (= . (primitive #<procedure:=>)) (- . (primitive #<procedure:->)) (> . (primitive #<procedure:>>)) (* . (primitive #<procedure:*>)) (< . (primitive #<procedure:<>)) (for-each . (procedure (proc items) ((if (null? items) (quote done) (begin (proc (car items)) (for-each proc (cdr items))))) #0#)))))

eval expr (quote (1 2 3))

'done
|#

;; part b
(define env2 (make-environment))
(eval '(define (p1 x)
         (set! x (cons x '(2)))
         x)
      env2)
(eval '(define (p2 x)
         (define (p e)
           e
           x)
         (p (set! x (cons x '(2)))))
      env2)
(eval '(p1 1) env2)
(eval '(p2 1) env2)

#|
With the original evaluator, we get '(1 2) since cons is a primitive and the argument x (a thunk) of cons
is forced, giving the value 1. The output of cons, which is '(1 2) is set as the new value of a variable x.
x is no longer a thunk at this point. Then x is eval'ed and returned.

In the second case, the argument of p2, x, is passed as a thunk. Now, the first statement, the definition
of the function p is defined. Now, this function is called with a (set! x ... ) in the second/final statement.
This statement is passed as a thunk (i.e. e is a thunk with expr (set! x ... )). As the first statement, e
is evaluated. e gets looked up and a thunk is returned. But the expr of thunk e is not evaluated. Now, we lookup
x and it is returned. x is the thunk with the expr `1'. This thunk is returned. If we don't force thunk in the 
repl (as in my examples where I call eval directly), what is returned is a thunk. x is not modified by the 
(set! x ...) because it is passed as thunk and has not been evaluated yet.

If we modify the eval-sequence according to Cy D's suggestions, then we get '(1 2) for both the statements. This is
because the first statement, e, gets forced, which makes the (set! x (cons x '(2))) to evaluate, creating the
side effect (x itself is a thunk, but gets forced because cons is a primitive). 

|#

;; part c
#|

This is because the display function is a primitive and it forces the thunk anyway.

|#

;; part d
#|

I like the approach in the text.

|#