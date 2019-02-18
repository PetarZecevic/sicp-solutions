#lang racket

(define tolerance 0.001)
; Pronalazi f(x) = x, na osnovu minimalne razlike dva sukcesivna clana f(x), f(f(x)),... za neku pocetnu vrednost x.
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;Pronalazak zlatnog odnosa na osnovu fiksne tacke.
(define (gold-function x) (+ 1 (/ 1 x)))