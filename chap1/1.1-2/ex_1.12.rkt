#lang racket

;Paskalov trougao:
;    1
;   1 1
;  1 2 1
; 1 3 3 1
;......... itd

;Procedura koja pronalazi broj koji se nalazi u n-tom redu i k-toj koloni Paskalovog trougla
(define (pascal n k)
  (if (or (< n 3) (< k 2) (> k (- n 1)))
      1
      (+ (pascal (- n 1) (- k 1))
         (pascal (- n 1) k))))