#lang racket

;Reformulisanje make-rat konstruktora tako da koriguje znake numeratora i denumeratora.
;Primer:
; -1/-1 = 1/1
; -1/1 = -1/1
; 1/-1 = -1/1
; 1/1 = 1/1

(define (make-rat n d)
  (define (gcd a b)
  (if (= b 0)
      a
      (gcd a (remainder a b))))
  (define (abs x) (if (< x 0) (- x) x))
  (let ((g (gcd n d)))
    (cond ((and (>= n 0) (>= d 0)) (cons (/ n g) (/ d g)))
          ((and (< n 0) (< d 0)) (cons (abs (/ n g)) (abs (/ d g))))
          (else (cons (- (abs (/ n g))) (abs (/ d g)))))))