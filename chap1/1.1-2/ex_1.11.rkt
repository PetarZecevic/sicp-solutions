#lang racket
;;Rekurzivan proces
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;Iterativan proces
(define (f1 n)
  (if (< n 3)
      n
      (f-iter 2 1 0 n 3)))

;Iterativni algoritam a=f(2), b=f(1), c=f(0), posle je a=f(3), b=f(2), c=f(1), itd..
(define (f-iter a b c max-count count)
  (if (> count max-count)
      a
      (f-iter (+ a (* 2 b) (* 3 c)) a b max-count (+ count 1))))