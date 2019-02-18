#lang racket

;Sinusna funkcija: sinx tezi ka x, kada je x dovoljno malo.
;sin(r) = 3*sin(r/3) - 4*sin^3(r/3), ugao je u radijanima
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sin angle)
  (if (< angle 0.1)
      angle
      (p (sin (/ angle 3.0)))))