#lang racket

;Linearan rekurzivan proces.
;Genericko mnozenje za dati interval
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;Iterativan proces, genericko mnozenje.
(define (prod-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial x)
  (define (identity x) x)
  (define (next x) (+ x 1))
  (product identity 1 next x))

(define (pi-product n) ;N - broj koraka, odnosi se na preciznost racunanja.
  (define (pi-term a) (/ (* a (+ a 2)) (* (+ a 1) (+ a 1))))
  (define (pi-next a) (+ a 2))
  (* 4.0 (prod-it pi-term 2 pi-next n)))