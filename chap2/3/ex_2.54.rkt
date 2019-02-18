#lang racket

;Porednje lista simbola.
(define (equal-1? x y)
  (cond ((xor (null? x) (null? y)) false)
        ((and (null? x) (null? y)) true)
        (else (let ((a (car x))
                    (b (car y)))
                (cond ((xor (pair? a) (pair? b)) false)
                      ((and (pair? a) (pair? b)) (equal-1? a b))
                      (else (and (eq? a b)
                                 (equal-1? (cdr x) (cdr y)))))))))