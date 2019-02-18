#lang racket

;Genericna suma, realizovana kao iterativan proces.
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;Procedure za probu.
(define (square x) (* x x))
(define (next x) (+ x 1))