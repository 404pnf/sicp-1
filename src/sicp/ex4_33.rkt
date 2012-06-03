#lang racket

(require "eval-4.33.rkt")

(define env1 (make-environment))
(eval '(define (cons a b)
              (lambda (m) (m a b)))
           env1)
(eval '(define (car z)
              (z (lambda (a b) a)))
           env1)
(eval '(define (cdr z)
              (z (lambda (a b) b)))
           env1)


(force-it (eval '(car (quote (1 2 3))) env1))
(force-it (eval '(car (quote (a b c))) env1))