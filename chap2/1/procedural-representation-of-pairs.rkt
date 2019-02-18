#lang racket

;Predstavljanje para, preko procedure.
(define (cons-p x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Greska: potrebno je uneti 0 ili 1"))))
  dispatch)

(define (car-p z) (z 0))
(define (cdr-p z) (z 1))

(define test (cons-p 1 5))
(newline)
(display (car-p test))
(display "/")
(display (cdr-p test))