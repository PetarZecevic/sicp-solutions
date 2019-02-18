#lang racket

;Ako primenimo normal-order evaluation pravilo, u if iskazima cemo morati oba
;izraza da evaluiramo pa cemo remainder operaciju primeniti x puta.
(define (gcd-normal a b)
  (define (if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
  (if (= b 0)
          a
          (gcd-normal b (remainder a b))))

;Ilustracija procesa
;(gcd 206 40)
;(if (= 40 0) 206 (gcd 40 (remainder 206 40)))
;(if #f 206 (gcd 40 6))
;(if #f 206 (if (= 6 0) 40 (gcd 6 (remainder 40 6))))
;(if #f 206 (if #f 40 (gcd 6 4)))
;(if #f 206 (if #f 40 (if (= 4 0) 6 (gcd 4 (remainder 6 4)))))
;(if #f 206 (if #f 40 (if #f 6 (gcd 4 2))))
;(if #f 206 (if #f 40 (if #f 6 (if (= 2 0) 4 (gcd 2 0)))))
;(if #f 206 (if #f 40 (if #f 6 (if #f 4 (if (= 0 0) 2 (gcd 0 (remainder 2 0))))))) -> ovde dolazi do greske jer se 0 nalazi kao argument u remainder operaciji


