#lang racket

;Rekurzivan proces za mnozenje preko sabiranja
(define (prod a b)
  (if (= b 0)
      0
      (+ a (prod a (- b 1)))))

;Iterativan proces za mnozenje preko sabiranja
(define (prod-iter product a b)
  (if (= b 0)
      product
      (prod-iter (+ product a) a (- b 1))))

;Pomocne procedure za optimizacije mnozenja preko sabiranja.
(define (double a) (+ a a))
(define (halve a) (/ a 2))

;Rekurzian proces za brzo mnozenje, koje je realizovano preko sabiranja
(define (prod-rec a b)
  (cond ((= b 0) 0)
        ((= (remainder b 2) 0) (double (prod-rec a (halve b))))
        (else (+ a (prod-rec a (- b 1))))))