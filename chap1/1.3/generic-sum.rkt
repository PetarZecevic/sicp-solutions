#lang racket

;Genericna suma.
;a - donja granica.
;b - gornja granica.
;term i next su procedure.
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

;Primena genericne sume.
;Suma kubova.
(define (sum-cubes a b)
  (define (cube x) (* x x x))
  (define (next x) (+ 1 x))
  (sum cube a next b))

(define (cube x) (* x x x))

;Koriscenje genericne sume kao gradivnog bloka u definisanju odredjenog integrala funkcije.
;int(a,b){f} = (f(a + dx/2) + f(a + dx + dx/2) + f(a + 2*dx + dx/2) + ...)*dx
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))