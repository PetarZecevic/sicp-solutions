#lang racket
;;Pomocne procedure
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

;;Linearni rekurzivni proces
(define (plus1 a b)
  (if (= a 0)
      b
      (inc (plus1 (dec a) b))))

;;Linearni iterativni proces
(define (plus2 a b)
  (if (= a 0)
      b
      (plus2 (dec a) (inc b))))