#lang racket

;Izmenjena genericna suma.
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

;Simpsonovo pravilo za nalazenje odredjenog integrala koriscenjem genericne sume.
;Granice su a i b, a <= b.
;N - broj koraka, koji se odnosi na preciznost aproksimacije.
(define (simpson-rule f a b n)
  (define h (* (/ (- b a) n) 1.0))
  (define (next k) (+ 1 k))
  (define (even? k) (= (remainder k 2) 0))
  (define (transform k)
    (cond ((or (= k 0) (= k n)) (f (+ a (* k h))))
          ((even? k) (* 2.0 (f (+ a (* k h)))))
          (else (* 4.0 (f (+ a (* k h)))))))
  (* (sum transform 0 next n)
     (/ h 3)))

;Funkcije za testiranje
(define (identity x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))