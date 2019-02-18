#lang racket

;Iterativan proces za brzo racunanje eksponenta baze.
(define (fast-exp-iter total base exp)
  (define (square x) (* x x))
  (cond ((< exp 1) total) 
        ((= (remainder exp 2) 0) (fast-exp-iter total (square base) (/ exp 2)))
        (else (fast-exp-iter (* total base) base (- exp 1)))))
