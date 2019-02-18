#lang racket

(define (enumerate-interval start end)
  (if (> start end)
      null
      (cons start (enumerate-interval (+ start 1) end))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append
              null
              (map proc seq)))

;Jedinstveni parovi brojeva (i,j) , tako da je 1 <= j < i <= n. 
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list j i)) (enumerate-interval 1 (- i 1))))(enumerate-interval 1 n)))

(unique-pairs 7)