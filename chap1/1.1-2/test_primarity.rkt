#lang racket
;Vraca #t ako je a deljivo sa b, u suprotnom #f.
(define (divide? a b)
  (= (remainder a b) 0))

(define (square x) (* x x))

;Pronalazi prvi delilac broja n pocesvi od k.
;Ako ga ne pronadje vraca n.
(define (find-divisors n k)
  (cond ((divide? n k) k)
        ((> (square k) n) n)
        (else (find-divisors n (+ k 1)))))

(define (smallest-divisor n)
  (find-divisors n 2))

;Broj je prost ako je deljiv samo sa samim sobom.
(define (prime? n)
  (= (smallest-divisor n) n))