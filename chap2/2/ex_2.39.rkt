#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;Desno odmotavanje.
(define fold-right accumulate)
;Levo odmotavanje.
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;Obrtanje liste definisano preko desnog odmotavanja.
(define (reverse-1 sequence)
  (fold-right (lambda (x y) (if (null? y) (list x) (append y (list x))))
              null
              sequence))
;Obrtanje liste definisano preko levog odmotavanja.
(define (reverse-2 sequence)
  (fold-left (lambda (x y) (cons y x))
             null
             sequence))

(define l-1 (list 1 2 3 4 5))
(reverse-1 l-1)
(reverse-2 l-1)