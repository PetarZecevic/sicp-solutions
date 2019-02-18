#lang racket

;Ne funkcionise kako treba jer pravi listu lista.
(define (square-list items)
  (define square (lambda (x) (* x x)))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))