#lang racket

(define (assert? expr) (tagged-list? expr 'assert))

(define (assert-predicate expr) (first (rest expr)))

(define (analyze-assert exp)
  (let ((pproc (analyze (assert-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                   (fail)
                   (succeed 'ok fail2)))
             fail))))