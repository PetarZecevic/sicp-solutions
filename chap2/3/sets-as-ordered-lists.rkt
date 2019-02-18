#lang racket

;Skupovi su uredjeni po rastucem redosledu elemenata.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;Kompleksnost O(n)
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1
                               (intersection-set (cdr set1)
                                                 (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) set2))
              ((< x2 x1) (intersection-set set1 (cdr set2)))))))

(element-of-set? 1 (list 1 3 4))
(element-of-set? 5 (list 1 3 4))
(element-of-set? 2 (list 1 3 4))
(element-of-set? 0 (list 1 3 4))

(intersection-set (list 1 2 3) (list 3 4 5))
(intersection-set (list 2 5 7) (list 4 5 6 7 8))