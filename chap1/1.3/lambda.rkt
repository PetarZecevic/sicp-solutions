#lang racket

(define plus4 (lambda (x) (+ x 4)))

;f(x,y) = x*(1 + x*y)^2 + y*(1-y) + (1-y)*(1+x*y)
;a = (1 + x*y)
;b = (1 - y)
;f(x,y) = x*a^2 + y*b + b*a
(define (f x y)
  ((lambda (a b) ;a i b su lokalne promenljive za olaksano racunanje.
     (+ (* x (* a a))
        (* y b)
        (* b a)))
     (+ 1 (* x y))
     (- 1 y)))

;Let konstrukcija za lokalne promenljive.
(define (f-l x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (* a a))
       (* y b)
       (* a b))))

;Koriscenjem internih definicija.
(define (f-int x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (* a a))
     (* y b)
     (* a b)))
