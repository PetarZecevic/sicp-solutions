#lang racket

;Mapiranje stabla, podrazumeva primenu procedura na svakom listu stabla i vracanje izmenjenog stabla.
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree))) tree))

(define t-1 (list (list 1 2 3) 4 (list 5 (list 6))))
(tree-map (lambda (x) (* x x x)) t-1)

;Kvadriranje stabla uspomoc procedure za mapiranje stabla.
(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))
(square-tree t-1)