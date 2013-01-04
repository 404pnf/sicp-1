#lang racket
(provide amb assert collect)

(define failures null)

(define (fail)
  (if (pair? failures)
      ((first failures))
      (error "no more choices!")))

(define (amb/thunks choices)
  (let/cc k (set! failures (cons k failures)))
  (if (pair? choices)
      (let ([choice (first choices)])
        (set! choices (rest choices))
        (choice))
      (begin (set! failures (rest failures))
             (fail))))

(define-syntax-rule (amb E ...)
  (amb/thunks (list (lambda () E) ...)))

(define (assert condition)
  (unless condition (fail)))

(define (collect/thunk n thunk)
  (define results null)
  (let/cc too-few
    (set! failures (list too-few))
    (define result (thunk))
    (set! results (cons result results))
    (set! n (sub1 n))
    (unless (zero? n) (fail)))
  (set! failures null)
  (reverse results))

(define-syntax collect
  (syntax-rules ()
    ;; collect N results
    [(_ N E) (collect/thunk N (lambda () E))]
    ;; collect all results
    [(_ E) (collect/thunk -1 (lambda () E))]))
