#lang racket

;Osnovne procedure
(define (square n) (* n n))

(define (even? n) (= (remainder n 2) 0))

;Modifikovana verzija tako da signalizira ako dodje do broja koji nije jednak sa 1 i n-1, a kongruentan sa 1 po modulu n.
(define (expmod base exp mod)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) mod)) mod))
        (else (remainder (* base (expmod base (- exp 1) mod)) mod))))
