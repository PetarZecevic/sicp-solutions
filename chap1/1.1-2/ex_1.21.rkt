#lang racket

;Pronalazi najmanji delilac za zadati broj.
;Iterativan proces.
(define (smallest-divisor num)
  (define (square x) (* x x)) ;Kvadriranje.
  (define (divisible? a b) (= (remainder a b) 0)) ;Proverava da li je a deljivo sa b.
  (define (find-divisor n k)
    (cond ((> (square k) n) n) ;Slucaj kada je broj prost, tada je deljiv samo sa samim sobom.
          ((divisible? n k) k) ;Pronasao prvog delioca
          (else (find-divisor n (+ k 1))))) ;Iterira dok ne pronadje delioca.
  (find-divisor num 2))