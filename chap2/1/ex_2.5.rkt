#lang racket

;Brza eksponencijacija broja.
(define (expt base exp)
  (define (even? x) (= (remainder x 2) 0))
  (define (iter result base exp)
    (cond ((= exp 0) result)
          ((even? exp) (iter result (* base base) (/ exp 2)))
          (else (iter (* result base) base (- exp 1)))))
  (iter 1 base exp))

;Racuna koliko puta mozemo da podelimo n sa divisor, tako da ostatak bude nula.
(define (count-divisions n divisor)
  (define (iter num count)
    (if (= (remainder num divisor) 0)
        (iter (/ num divisor) (+ count 1))
        count))
  (iter n 0))

;Par (a,b) implementiran preko broja 2^a*3^b.
(define (cons a b) (* (expt 2 a) (expt 3 b)))
(define (car z) (count-divisions z 2))
(define (cdr z) (count-divisions z 3))