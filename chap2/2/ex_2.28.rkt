#lang racket

;Vraca listove stabla, poredjane po redosledu u listu.
(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? (car tree))) (cons (car tree)
                                        (fringe (cdr tree))))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))