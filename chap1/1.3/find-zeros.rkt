#lang racket

;Generalni metod za nalazenje nula funkcije.
(define (find-zero function a b tolerance)
  (let ((x (/ (+ a b) 2.0))
        (length (- b a)))
    (cond ((or (<= length tolerance) (= (function x) 0)) x)
          ((> (function x) 0) (find-zero function a x tolerance))
          (else (find-zero function x b tolerance)))))

;Pomocne procedure za search proceduru.
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (close-enough? x y)
  (<= (abs (- x y)) 0.001))
(define (average x y)
  (/ (+ x y) 2.0))

;Metod iz knjige.
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

;Generalni metod za pronalazak nula funkcije koji proverava da li je interval dobro podesen.
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (positive? a-value) (negative? b-value)) (search f b a))
          ((and (negative? a-value) (positive? b-value)) (search f a b))
          (else (error "Vrednosti funkcije u datim tackama nisu suprotnog znaka" a b)))))
