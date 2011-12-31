#lang racket

;; The text version creates something like this

(lambda (env)
  (lambda (env)
    (lambda (env)
      (lambda (env)
        (proc1 env)
        (proc2 env))
      (proc3 env))
    (proc4 env)))

;; where as Alyssa's version creates
(lambda (env) (execute-sequence '(proc1 proc2 proc3 ...) env))