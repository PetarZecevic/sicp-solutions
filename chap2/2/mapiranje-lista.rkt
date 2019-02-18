#lang racket

;Korisna stvar je izvrsiti transformaciju nad svakim elementom liste i transformisane elemente vratiti kao novu listu.
(define (scale-list items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
        (scale-list (cdr items) factor))))

;Generalnija procedura za mapiranje mape koja uzima listu elemenata i proceduru koja predstavlja transformaciju nad svakim elementom.
(define (map items transform)
  (if (null? items)
      null
      (cons (transform (car items))
            (map (cdr items) transform))))

;Procedura skaliranja liste izrazena preko mapiranje liste.
(define (scale-list-m items factor)
  (map items (lambda (x) (* x factor))))

;Sabira korespodentne elemente dve liste, liste moraju biti iste velicine.
(define (list-add list-1 list-2)
  (if (or (null? list-1) (null? list-2))
      null
      (cons (+ (car list-1) (car list-2))
            (list-add (cdr list-1) (cdr list-2)))))