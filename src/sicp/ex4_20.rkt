#lang racket

#|
(define (f x)
  (letrec ((even? 
            (lambda (n)
              (if (= n 0)
                  #t
                  (odd? (- n 1)))))
           (odd?
            (lambda (n)
              (if (= n 1)
                  #t
                  (even? (- n 1))))))
    <body>))



(letrec ...) is transformed into:

(let ((even? '*unassigned*)
      (odd? '*unassigned*))
  (set! even? (lambda (n)
                (if (= n 0)
                    ...
                    ...)))
  (set! odd? (lambda (n)
               ...))
  <body>)

|#

#|

letrec is of the form:

(letrec ((<var1> <exp1>) ... (<varn> <expn>))
  <body>)

|#
(define (tagged-list? expr tag)
  (or (pair? expr) (eq? (car expr) tag)))

(define (letrec? expr)
  (tagged-list? expr 'letrec))

(define (letrec-variables expr)
  (let ((p (car (cdr expr))))
    (map car p)))

(define (letrec-values expr)
  (let ((p (car (cdr expr))))
    (map cadr p)))

(define (letrec->let expr)
  (if (not (letrec? expr))
      (error "not a letrec expression -- LETREC")
      (let ((vars (letrec-variables expr))
            (vals (letrec-values expr))
            (body (cdr (cdr expr))))
        (cons 'let
              (cons (map (lambda (var)
                           (list var ''*unassigned*))
                         vars)
                    (append (map (lambda (var val)
                                   (list 'set! var val))
                                 vars
                                 vals)
                            body))))))

#|

b. In the case where we use lecrec, this gets transformed into let. So, when we call (f 5)
a new frame is formed with even? and odd? assigned to '*unassigned. Then these are set to 
the lambda expressions belonging to even? and odd?.

When we use a let in the place of letrec, a frame gets created with even? and odd? assigned
to the lambda expressions. Then the body of the let expression (i.e. <rest of body of f>)
is wrapped in a lambda which takes as parameters, even? and odd? is called with the lambda
expressions corresponding to even? and odd?. 

i.e. ((lambda (e? o?)
       <rest of body of f>) 
      (lambda (n) (...reference to o?..)) 
      (lambda (n) (.reference to e?... )))

So, a frame gets created which assigns e? and o? to the corresponding lambdas. The enclosing
environment for it, does not have e? or o? defined. So, these definitions
When we draw the environment, the two lambdas passed as parameters do not see each other as their
environment is the enclosing env of t. so they are undefined. When the body is evaluated, then
these will generate error for undefined references.

|#