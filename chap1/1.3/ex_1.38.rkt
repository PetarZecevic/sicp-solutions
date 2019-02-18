#lang racket

(define (cont-frac n d k)
  (define (iter result k)
    (let ((s (/ (n k) (+ (d k) result))))
      (if (= k 0)
          result
         (iter s
              (- k 1)))))
  (iter (/ (n k) (d k)) (- k 1)))

;Ojlerova aproksimacija za e.
;K se odnosi na preciznost aproksimacije.
(define (e-aprox k)
  (+ 2 (cont-frac
        (lambda (i) 1.0)
        (lambda (i)
          (let ((divide-3? (= (remainder (- i 2) 3) 0))
                (k (/ (- i 2) 3)))
            (if divide-3?
                (* (+ k 1) 2)
                1)))
        k)))