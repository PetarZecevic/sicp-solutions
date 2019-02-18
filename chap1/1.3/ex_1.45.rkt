#lang racket

(define tolerance 0.001)
(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try a)
    (if (close-enough? (f a) a)
        a
        (try (f a))))
  (try guess))

(define (compose f g)
  (lambda (x) (f (g x))))

;Primenjuje proceduru f na argument x, n-puta.
;Iterativan proces, logaritamske kompleksnosti.
(define (even? n) (= (remainder n 2) 0))
(define (repeated f n)
  (define (rep proc times)
    (cond ((<= times 1) proc)
          ((even? n) (rep (compose proc proc) (/ times 2)))
          (else (rep (compose f proc) (- times 1)))))
  (rep f n))

(define (average x y)
  (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

;Iterativan proces za racunanje eksponenta broja, logaritamske kompleksnosti.
(define (exp base n)
  (define (exp-iter result x n)
    (cond ((= n 0) result)
          ((even? n) (exp-iter result (* x x) (/ n 2)))
          (else (exp-iter (* result x) x (- n 1)))))
  (exp-iter 1 base n))

;Nalazenje n-tog korena broja.
(define (root n x)
  (fixed-point ((repeated average-damp (if (even? n) (/ (log n) (log 2)) (/ (+ n 1) 2)))(lambda (y) (/ x (exp y (- n 1))))) 1.0))