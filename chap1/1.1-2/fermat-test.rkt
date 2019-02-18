#lang racket

;Ovo sam prekucao sa pdf-a, nisam dobro razumeo Fermatovu malu teoremu.

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

;Ovo racuna base^exp(mod m).
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                              m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)) ;Da li je ostatak pri deljenju a^n sa n jednako a.
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1))) ;Ako prodje, nastavlja da testira.
        (else false)))
