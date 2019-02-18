#lang racket

(define (even? n) (= (remainder n 2) 0))
(define (square n) (* n n))

;Iterativan proces za eksponencijaciju, logaritamske kompleksnosti.
(define (fast-expt base exp)
  (define (exp-iter res base exp)
    (cond ((= exp 0) res)
          ((even? exp) (exp-iter res (square base) (/ exp 2)))
          (else (exp-iter (* res base) base (- exp 1)))))
  (exp-iter 1 base exp))

;Iterativan proces za odredjivanje ostatka pri deljenje base^exp sa mod.
(define (expmod base exp mod)
  (remainder (fast-expt base exp) mod))