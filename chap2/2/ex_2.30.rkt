#lang racket

(define square (lambda (x) (* x x)))

;Direktna implementacija.
(define (square-tree-1 tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define t-1 (list (list 1 2) 3 (list 4 (list 5 6))))
(square-tree-1 t-1)

;Implementacija koriscenjem procedura viseg reda.
;Koristi se primitivna procedura map, koja mapira sadrzaj liste.
(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (square sub-tree))) tree))

(square-tree-2 t-1)