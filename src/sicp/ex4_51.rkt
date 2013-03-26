#lang racket

;;; assignment
(define (analyze-assignment expr)
  (let ([var (assignment-var expr)]
        [vproc (analyze (assignment-value expr))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ([old-val (lookup-variable-value var env)])
                 (set-variable-value! var val)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-val env)
                            (fail2)))))
             fail))))

;; permenent-set!
;;; assignment
(define (analyze-permanent-assignment expr)
  (let ([var (perm-assignment-var expr)]
        [vproc (analyze (perm-assignment-value expr))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val)
               (succeed 'ok fail2))
             fail))))
