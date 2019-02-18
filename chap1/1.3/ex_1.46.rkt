#lang racket

;Generalizacija iterativnih procesa u vidu apstraktne konstrukcije iterative-improvement koja vraca proceduru,
;koja iterira i u svakom koraku, pobosljava pocetnu vrednost dok se uslov ne ispuni.
(define (iterative-improve improve good-enough?)
  (lambda (x)
    (define (iter x)
      (if (good-enough? x)
          x
          (iter (improve x))))
    (iter x)))


(define (fixed-point f guess)
  (let ((tolerance 0.0001))
    ((iterative-improve (lambda (x) (/ (+ x (f x)) 2)) (lambda (x) (< (abs (- x (f x))) tolerance))) guess)))