#lang racket

;Modifikovan fixed-point tako da stampa medjurezultate.
(define tolerance 0.001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;Tehnika za brzu konvergenciju fiksnih tacaka.
(define (average x y) (/ (+ x y) 2))
(define (average-damping f)
  (lambda (x) (average x (f x))))