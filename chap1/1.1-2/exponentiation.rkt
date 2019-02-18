#lang racket

;Rekurizan proces za racunanje eksponenta
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;Iterativan proces za racunanje eksponenta
(define (expt1 base exponent)
  (define (exp-iter total exp)
    (if (= exp 0)
        total
        (exp-iter (* total base) (- exp 1))))
  (exp-iter 1 exponent))

;Brzo racunanje eksponenta, uspomoc deljenja parnog eksponenta
(define (fast-exp b n)
  (cond ((< n 2) b)
        ((= (remainder n 2) 0) (fast-exp (* b b) (/ n 2)))
        (else (* b (fast-exp b (- n 1))))))