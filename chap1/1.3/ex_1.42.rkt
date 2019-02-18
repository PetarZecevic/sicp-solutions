#lang racket

;Kompozicija funkcija.
(define (compose f g)
  (lambda (x) (f (g x))))