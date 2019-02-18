#lang racket

;Procedura koja racuna duzinu liste.
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;Iterativan proces za racunanje duzine liste.
(define (length-it items)
  (define (iter items l)
    (if (null? items)
        l
        (iter (cdr items) (+ l 1))))
  (iter items 0))

;Procedura koja racuna broj listova u stablu.
;pair? je primitivni operator koji vraca true, ako mu je prosledjen par.
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;Stablo predstavljamo preko lista koje sadrze liste unutar sebe, a te liste opet mogu sadrzi liste, sve do listova koji predstavljaju konrektne vrednosti.
(define stablo-1 (list (list 1 2 3) 1 2))
(define stablo-2 (list (list (list 1 2 3) (list 5 7) (list 9 10 11))))
(define stablo-3 (list 1 2 3 (list 4 5 6)))