#lang racket

;Primena lambda racuna.
;Konstrukcija para, preko lambda funkcija.
(define (cons x y)
  (lambda (m) (m x y)))
;Uzimanje prvog dela para.
(define (car z)
  (z (lambda (p q) p)))
;Uzimanje drugog dela para.
(define (cdr z)
  (z (lambda (p q) q)))

(define test (cons 1 5))