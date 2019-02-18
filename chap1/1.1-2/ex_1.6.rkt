#lang racket
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (square x) (* x x))
(define (<= x y)
  (not (> x y)))
(define (good-enough? guess x)
  (<= (abs (- (square guess) x)) 0.001))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
;; If uradjen kao obicna procedura.
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;; Ako se ovo pokrene ulazi se u beskonacnu petlju, jer se new-if
; implementirano kao obicna procedura, pa podizraz square-iter mora da se evaluira
; sto ce dovesti do rekurzije, koja nema izlaza.
(define (square-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (square-iter (improve guess x) x)))
(define (sqrt x)
  (square-iter 1.0 x))