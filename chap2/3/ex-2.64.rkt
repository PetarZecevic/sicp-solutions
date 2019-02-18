#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;Formiranje balansiranog stabla na osnovu sortirane liste u rastucem poretku.
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;Kompleksnost procedure je O(n).
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(list->tree (list 1 3 5 7 9 11))
(list->tree (list 1 2 3))
(list->tree (list 1 2 3 4))
(list->tree (list 1 2 3 4 5))
(list->tree (list 1 2 3 4 5 6))
(list->tree (list 1 2 3 4 5 6 7))