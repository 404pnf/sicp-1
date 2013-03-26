#lang racket

;; if-fail

(define (analyze-if-fail expr)
  (let ([sproc (analyze (if-fail-success expr))]
        [fproc (analyze (if-fail-failure expr))])
    (lambda (env succeed fail)
      (sproc env
             succeed
             (lambda ()
               (fexpr env succeed fail))))))