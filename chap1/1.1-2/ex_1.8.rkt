#lang racket
;; Njutnov metod za kubne korene.
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (<= x y)
  (not (> x y)))

(define (square x)
  (* x x))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough? curr-iter next-iter)
  (<= (abs (- curr-iter next-iter))
      0.001))

(define (cube-root-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (cube-root-iter (improve guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))