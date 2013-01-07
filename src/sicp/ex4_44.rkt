#lang racket

(require "amb-eli.rkt")
(require "distinct.rkt")

(define (queens)
  (let ([q1r (amb 1 2 3 4 5 6 7 8)]
        [q1c (amb 1 2 3 4 5 6 7 8)]
        [q2r (amb 1 2 3 4 5 6 7 8)]
        [q2c (amb 1 2 3 4 5 6 7 8)])
    (assert (distinct? (list q1r q2r)))
    (assert (distinct? (list q1c q2c)))
    (assert (not (diagonal? (list q1r q1c) (list q2r q2c))))
    (let ([q3r (amb 1 2 3 4 5 6 7 8)]
          [q3c (amb 1 2 3 4 5 6 7 8)])
      (for ([qr (list q1r q2r)])
        (assert (distinct? (list q3r qr))))
      (for ([qc (list q1c q2c)])
        (assert (distinct? (list q3c qc))))
      (for ([q (list (list q1r q1c) (list q2r q2c))])
        (assert (not (diagonal? (list q3r q3c) q))))
      (let ([q4r (amb 1 2 3 4 5 6 7 8)]
            [q4c (amb 1 2 3 4 5 6 7 8)])
        (for ([qr (list q1r q2r q3r)])
          (assert (distinct? (list q4r qr))))
        (for ([qc (list q1c q2c q3c)])
          (assert (distinct? (list q4c qc))))
        (for ([q (list (list q1r q1c) (list q2r q2c) (list q3r q3c))])
          (assert (not (diagonal? (list q4r q4c) q))))
        (let ([q5r (amb 1 2 3 4 5 6 7 8)]
              [q5c (amb 1 2 3 4 5 6 7 8)])
          (for ([qr (list q1r q2r q3r q4r)])
            (assert (distinct? (list q5r qr))))
          (for ([qc (list q1c q2c q3c q4c)])
            (assert (distinct? (list q5c qc))))
          (for ([q (list (list q1r q1c) (list q2r q2c) (list q3r q3c) (list q4r q4c))])
            (assert (not (diagonal? (list q5r q5c) q))))
          (let ([q6r (amb 1 2 3 4 5 6 7 8)]
                [q6c (amb 1 2 3 4 5 6 7 8)])
            (for ([qr (list q1r q2r q3r q4r q5r)])
              (assert (distinct? (list q6r qr))))
            (for ([qc (list q1c q2c q3c q4c q5c)])
              (assert (distinct? (list q6c qc))))
            (for ([q (list (list q1r q1c) (list q2r q2c) (list q3r q3c) (list q4r q4c) (list q5r q5c))])
              (assert (not (diagonal? (list q6r q6c) q))))
            (let ([q7r (amb 1 2 3 4 5 6 7 8)]
                  [q7c (amb 1 2 3 4 5 6 7 8)])
              (for ([qr (list q1r q2r q3r q4r q5r q6r)])
                (assert (distinct? (list q7r qr))))
              (for ([qc (list q1c q2c q3c q4c q5c q6c)])
                (assert (distinct? (list q7c qc))))
              (for ([q (list (list q1r q1c) (list q2r q2c) (list q3r q3c) (list q4r q4c) (list q5r q5c) (list q6r q6c))])
                (assert (not (diagonal? (list q7r q7c) q))))
              (let ([q8r (amb 1 2 3 4 5 6 7 8)]
                    [q8c (amb 1 2 3 4 5 6 7 8)])
                (for ([qr (list q1r q2r q3r q4r q5r q6r q7r)])
                  (assert (distinct? (list q8r qr))))
                (for ([qc (list q1c q2c q3c q4c q5c q6c q7c)])
                  (assert (distinct? (list q8c qc))))
                (for ([q (list (list q1r q1c) (list q2r q2c) (list q3r q3c) (list q4r q4c) (list q5r q5c) (list q6r q6c) (list q7r q7c))])
                  (assert (not (diagonal? (list q8r q8c) q))))
                (list (list 'q1 q1r q1c)
                      (list 'q2 q2r q2c)
                      (list 'q3 q3r q3c)
                      (list 'q4 q4r q4c)
                      (list 'q5 q5r q5c)
                      (list 'q6 q6r q6c)
                      (list 'q7 q7r q7c)
                      (list 'q8 q8r q8c))))))))))

(define (diagonal? p1 p2)
  (let ([x1 (first p1)]
        [y1 (second p1)]
        [x2 (first p2)]
        [y2 (second p2)])
    (= (abs (- x1 x2))
       (abs (- y1 y2)))))

(queens)

;; '((q1 1 1) (q2 2 5) (q3 3 8) (q4 4 6) (q5 5 3) (q6 6 7) (q7 7 2) (q8 8 4))