#lang racket

;Izdvajanje odredjenog lista iz stabla upotrebom car i cdr operatora.
;Konkretno u nasem slucaju izdvajamo broj 7.
(define t-1 (list 1 3 (list 5 7) 9))
(define t-2 (list (list 7)))
(define t-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;Prvo stablo.
(car (cdr (car (cdr (cdr t-1)))))
;Drugo stablo.
(caar t-2)
;Trece stablo.
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr t-3))))))))))))

(define x (list 1 2 3))
(define y (list 4 5 6))
(cons x y)
(list x y)