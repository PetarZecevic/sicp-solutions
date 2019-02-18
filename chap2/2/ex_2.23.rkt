#lang racket

(define (for-each proc items)
  (cond ((not (null? items)) (proc (car items)) (for-each proc (cdr items)))))

(define print-nice (lambda (x) (newline) (display x)))

;Generise listu brojeva koja predstavlja pocevsi od start-number do end-number.
(define (generate-interval start-number end-number)
  (define (guard-iter a b interval)
    (if (> a b)
        interval
        (guard-iter a
                    (- b 1)
                    (cons b interval))))
  (guard-iter (min start-number end-number)
              (max start-number end-number)
              null))