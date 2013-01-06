#lang racket

(require "amb-eli.rkt")
(require "distinct.rkt")

#|

Fathers: 

1. Moore  - yatch - lorna
2. Colonel Downing - melissa
3. Mr. Hall - rosalind
4. Sir Barnacle Hood (Melissa's father) - gabreille
5. Dr. Parker - mary ann moore

Daughters

1. Mary Ann Moore
2. Gabrielle
3. Lorna
4. Rosalind
5. Melissa's father is Sir Barnacle Hood

|#

(define (get-yatch father)
  (case (string->symbol (string-append (symbol->string father) "-y"))
    [(moore-y) 'lorna]
    [(downing-y) 'melissa]
    [(hall-y) 'rosa]
    [(barnacle-y) 'gab]
    [(parker-y) 'mary]))

(define (father x) (first x))
(define (daughter x) (second x))
(define (yat x) (third x))

(define (yatch)
  (let ([moore (list (amb 'mary) 'lorna)]
        [downing  (list 'downing (amb 'gab 'lorna 'rosa) 'melissa)]
        [hall (list 'hall (amb 'gab 'lorna) 'rosa)]
        [barnacle (list 'barnacle (amb 'melissa) 'gab)]
        [parker (list 'parker (amb 'gab 'lorna 'rosa) 'mary)])
    (let ([gab-father (amb hall downing parker)]
          [lorna-father (amb hall downing parker)])
      (assert (eq? (daughter gab-father) 'gab))
      (assert (eq? (daughter lorna-father) 'lorna))
      (assert (eq? (yat gab-father) (daughter parker)))
      (assert (not (eq? (daughter lorna-father) (yat lorna-father))))
      (assert (not (eq? (daughter gab-father) (yat gab-father))))
      lorna-father)))

(yatch)
