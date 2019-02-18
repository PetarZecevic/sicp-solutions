#lang racket

;Generalnija verzija akumulatora, koja ukljucuje filter.
;Filter odredjuje koje vrednosti, iz datog opsega, treba da kombinuje kao term-ove.
(define (filtered-accumulate filter combiner null-value term a next b)
   (cond ((> a b) null-value)
         ((filter a) (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))
         (else (combiner null-value (filtered-accumulate filter combiner null-value term (next a) next b)))))

;Suma kvadrata prostih brojeva u odredjenom opsegu.
(define (square x) (* x x))
(define (prime? n)
  (define (prime-iter a b)
    (cond ((> (square a) b) true)
          ((= (remainder b a) 0) false)
          (else (prime-iter (+ a 1) b))))
  (prime-iter 2 n))

(define (sum-prime-squares a b)
  (define (next a) (+ a 1))
  (filtered-accumulate prime? + 0 square a next b))

(define (relative-prime-products a b)
  (define (gcd n k)
    (if (= k 0)
        n
        (gcd k (remainder n k))))
  (define (relative-prime? k)
    (= (gcd b k) 1))
  (define (identity k) k)
  (define (next k) (+ k 1))
  (filtered-accumulate relative-prime? * 1 identity a next (- b 1)))