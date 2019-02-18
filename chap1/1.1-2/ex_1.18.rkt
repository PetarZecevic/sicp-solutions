#lang racket

;Iterativan proces za brzo mnozenje, koje je realizovano preko sabiranja
(define (prod-fast result a b)
  (define (double a) (+ a a))
  (define (halve a) (/ a 2))
  (cond ((= b 0) result)
        ((= (remainder b 2) 0) (prod-fast result (double a) (halve b)))
        (else (prod-fast (+ result a) a (- b 1)))))