#lang racket

;Euklidov algoritam za najveci zajednicki brojilac brojeva.
;GCD(a, b) -> GCD(b, r), gde je r ostatak pri deljenju broja a sa b.
;Iterativan proces.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))