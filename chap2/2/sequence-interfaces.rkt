#lang racket

;Suma kvadrata neparnih listova stabla.
(define (sum-odd-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) ;Primitivna procedura.
             (* tree tree)
             0))
        (else (+ (sum-odd-leaves (car tree))
                 (sum-odd-leaves (cdr tree))))))

(define t-1 (list (list 1 2 3) 4 5 (list 6 (list 8))))
(sum-odd-leaves t-1)

;Konstuise lisu svih parnih fibonacijevih brojeva.
(define (even-fibs n)
  (define (fib n)
    (if (or (= n 0) (= n 1))
        1
        (+ (fib (- n 1))
           (fib (- n 2)))))
  (define (next k)
    (let ((f (fib k)))
      (if (> k n)
        null
        (if (even? f)
            (cons f (next (+ k 1)))
            (next (+ k 1))))))
  (next 0))