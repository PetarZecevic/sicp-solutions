#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;Kompleksnost O(log2n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))))

(define t1 (make-tree 5 (make-tree 3 (make-tree 2 '() '()) '()) (make-tree 7 '() (make-tree 9 '() '()))))

(element-of-set? 5 t1)
(element-of-set? 2 t1)
(element-of-set? 9 t1)
(element-of-set? 1 t1)

(adjoin-set 4 t1)
(adjoin-set 6 t1)
(adjoin-set 1 t1)
(adjoin-set 3 t1)
(adjoin-set 10 t1)

(define t2 (make-tree 1 '() '()))
(adjoin-set 8
            (adjoin-set 7
                        (adjoin-set 6
                                    (adjoin-set 5
                                                (adjoin-set 4
                                                            (adjoin-set 3
                                                                        (adjoin-set 2 t2)))))))
