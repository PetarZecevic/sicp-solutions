#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;Konverzija stabla u listu.
;Levi obilazak stabla, tj. prvo se obidje svaki levi pa desni cvor.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

;Desni obilazak stabla, tj. prvo se obidje svaki desni pa levi cvor.
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define t1 (make-tree 3 (make-tree 2 (make-tree 1 '() '()) '()) (make-tree 4 '() '())))
(define t2 (make-tree 5 '() (make-tree 6 '() '())))
(tree->list-1 t1)
(tree->list-1 t2)
(tree->list-2 t1)
(tree->list-2 t2)