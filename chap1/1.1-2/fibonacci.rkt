#lang racket

;Stablo rekurzivan proces
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

;Iterativan proces
(define (fib-iter first-sum second-sum counter)
  (if (= counter 0)
      first-sum
      (fib-iter second-sum (+ first-sum second-sum) (- counter 1))))