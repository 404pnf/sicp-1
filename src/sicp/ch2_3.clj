(ns sicp.ch2_3
  (:use [sicp.utils :only (error)]
        [sicp.ex2_54 :only (equal?)]))

(defn memq [item x]
  (cond
    (empty? x) false
    (= (first x) item) x
    :else (memq item (rest x))))

;; differentiation

;; take it for granted the following primitives.
(declare variable? same-variable? sum? addend augend make-sum product? make-product multiplier multiplicand)

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum (make-product (multiplier exp)
                                               (deriv (multiplicand exp) var))
                                 (make-product (deriv (multiplier exp) var)
                                               (multiplicand exp)))
        :else (error "unknown expression type -- derive")))

(defn variable? [x]
  (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(defn sum? [x]
  (and (list? x) (= (first x) '+)))

(defn addend [s]
  (second s))

(defn augend [s]
  (second (rest s)))

(defn product? [x]
  (and (list? x) (= (first x) '*)))

(defn multiplier [p]
  (second p))

(defn multiplicand [p]
  (second (rest p)))

;;;; 2.3.3 sets
(defn element-of-set? [x set]
  (cond (empty? set) false
        (equal? x (first set)) true
        :else (element-of-set? x (rest set))))

;; add an element to the set, if not already part of the set and return the set. If
;; already part of the set, then return the set
(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

;; intersection of two sets (i.e. elements of the set which are present in both the
;; sets)
(defn intersection-set [set1 set2]
  (cond (or (empty? set1) (empty? set2)) ()
        (element-of-set? (first set1) set2) (cons (first set1)
                                                  (intersection-set (rest set1) set2))
        :else (intersection-set (rest set1) set2)))


;;; sets as ordered list
(defn element-of-set? [x set]
  (cond (empty? set) false
        (= (first set) x) true
        (< x (first set)) false
        :else (element-of-set? x (rest set))))

(defn intersection-set [set1 set2]
  (if (or (empty? set1) (empty? set2))
    ()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond (= x1 x2) (cons x1 (intersection-set (rest set1)
                                                 (rest set2)))
            (< x1 x2) (intersection-set (rest set1) set2)
            (< x2 x1) (intersection-set (rest set2) set1)))))

;;; sets as trees
;;; trees using lists
;;;  every node is a list of 3 elements: entry, left tree and right tree
(defn entry [tree]
  (first tree))

(defn left-branch [tree]
  (second tree))

(defn right-branch [tree]
  (second (rest tree)))

(defn make-tree [entry left right]
  (list entry left right))

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= (entry set) x) true
        (< x (entry set)) (element-of-set? x (left-branch set))
        (> x (entry set)) (element-of-set? x (right-branch set))))

(defn adjoin-set [x set]
  (cond (empty? set) (make-tree x '() '())
        (= x (entry set)) set
        (< x (entry set)) (make-tree (entry set)
                                     (adjoin-set x (left-branch set))
                                     (right-branch set))
        (> x (entry set)) (make-tree (entry set)
                                     (left-branch set)
                                     (adjoin-set x (right-branch set)))))


;;; key lookup
(defn lookup [given-key set-of-records]
  (cond (empty? set-of-records) false
        (equal? given-key (key (first set-of-records))) (first set-of-records)
        :else (lookup given-key (rest set-of-records))))