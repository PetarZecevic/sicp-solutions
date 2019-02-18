#lang racket

;Osnovne procedure
(define (square n) (* n n))

(define (even? n) (= (remainder n 2) 0))

(define (expmod base exp mod)
  (cond ((= exp 0) 1)
        ((= exp 2) )
        ((even? exp) (remainder (square (expmod base (/ exp 2) mod)) mod))
        (else (remainder (* base (expmod base (- exp 1) mod)) mod))))

;Testira da li je za svako a < n, ostatak pri deljenju a^n sa n jednako a.
(define (test n)
  (define (test-iter count n mod)
    (cond ((= count 0) true)
          ((= (expmod count n mod) count) (test-iter (- count 1) n mod))
          (else false)))
  (test-iter (- n 1) n n))