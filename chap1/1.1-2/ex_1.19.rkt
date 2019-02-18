#lang racket

;Iterativan proces za odredjivanje Fibonacijev brojeva, koji je logaritamske kompleksnosti.
(define (fib n)
  (define (even? n) (= (remainder n 2) 0))
  (define (fib-iter a b p q count)
    (cond ((= count 0) b) ;b = Fib(n), a = Fib(n+1)
          ((even? count) (fib-iter a ;Dupla transformacija za parno n.
                                   b
                                   (+ (* p p) (* q q)) ;p'
                                   (+ (* q q) (* 2 p q)) ;q'
                                   (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))