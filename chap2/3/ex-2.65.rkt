#lang racket

;Unija uredjenih lista.
(define (union-set-l set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2) (cons x1
                                       (union-set-l (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1
                                       (union-set-l (cdr set1) set2)))
                      ((< x2 x1) (cons x2
                                       (union-set-l set1 (cdr set2)))))))))

;Presek uredjenih lista.
(define (intersection-set-l set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1
                               (intersection-set-l (cdr set1)
                                                 (cdr set2))))
              ((< x1 x2) (intersection-set-l (cdr set1) set2))
              ((< x2 x1) (intersection-set-l set1 (cdr set2)))))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
              (cons (entry tree)
                    (tree->list (right-branch tree))))))

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

;Unija skupova koji su realizovani preko balansiranih stabala.
(define (union-set set1 set2)
  (list->tree (union-set-l (tree->list set1)
                          (tree->list set2))))

;Presek skupova koji su realizovani preko balansiranih stabala.
(define (intersection-set set1 set2)
  (list->tree (intersection-set-l (tree->list set1)
                                  (tree->list set2))))

(union-set (list->tree (list 1 2 3 4 5))
           (list->tree (list 4 5 6 7 8)))

(intersection-set (list->tree (list 1 2 3 4 5))
                  (list->tree (list 3 4 5 6 7 8)))