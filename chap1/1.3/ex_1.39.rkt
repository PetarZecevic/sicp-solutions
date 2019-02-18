#lang racket

(define (cont-frac n d k)
  (define (iter result k)
    (let ((s (/ (n k) (+ (d k) result))))
      (if (= k 0)
          result
         (iter s
              (- k 1)))))
  (iter (/ (n k) (d k)) (- k 1)))

;Lambertova aproksimacija za tan(x)
(define (tan-cf x k)
  (cont-frac
   (lambda (i)
     (if (= i 1)
         x
         (- 0 (* x x))))
   (lambda (i) (- (* 2 i) 1))
   k))