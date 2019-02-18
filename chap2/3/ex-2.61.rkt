#lang racket

;Kompleksnost O(n)
(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((first (car set)))
        (cond ((= x first) set)
              ((< x first) (cons x set))
              ((> x first) (cons first
                                 (adjoin-set x (cdr set))))))))

(adjoin-set 4 (list 1 3 5))
(adjoin-set 6 (list 1 2 3))
(adjoin-set 3 (list 1 2 3 4 5 6))