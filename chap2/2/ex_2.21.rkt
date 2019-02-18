#lang racket

;Razliciti nivoi apstrakcije procedura u zavisnosti od nacina implementacije.
(define (square-list-1 items)
  (if (null? items)
      null
      (cons (* (car items) (car items))
            (square-list-1 (cdr items)))))

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))