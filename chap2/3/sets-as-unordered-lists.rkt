#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '() )
        ((element-of-set? (car set1) set2) (cons (car set1)
                                                (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(element-of-set? 1 (adjoin-set 1 (list 2 3)))
(element-of-set? 1 (adjoin-set 1 (list 1 2 3)))
(element-of-set? 1 (adjoin-set 1 '() ))
(intersection-set (list 1 2 3) '() )
(intersection-set (list 1 2 3) (list 1 2 3))
(intersection-set (list 1 2 3) (list 1))
(intersection-set (list 1 2) (list 1 2 3))