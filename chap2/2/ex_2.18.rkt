#lang racket

;Pristupanje elementu liste preko indeksa.
(define (list-ref arr n)
  (if (= n 0)
      (car arr)
      (list-ref (cdr arr) (- n 1))))

;Konkatenacija lista.
(define (append-1 list-1 list-2)
  (if (null? list-1)
      list-2
      (cons (car list-1) (append-1 (cdr list-1) list-2))))

;Vraca listu u obrnutom redosledu.
;Prvi nacin:
; 1. Lista sa jednim elementom je vec obrnuta.
; 2. Listu koja ima vise od jednog elementa obrcemo tako sto vrsimo konkatenaciju, liste bez prvog elementa sa listom koja sadrzi prvi element.
;--------------------
; 1. je bazni slucaj.
; 2. je rekurzivni slucaj.
;Rekurzivan proces.
(define (reverse-1 arr)
  (if (null? (cdr arr)) ;Lista sa jednim elementom je vec obrnuta.
      arr
      (append-1 (reverse-1 (cdr arr)) (list (car arr))))) ;Mora da se napravi lista sa jednim elementom jer car vraca element a ne listu.
;Alternativa za (list (car arr)) bi bila (cons (car arr) null).


;Procedure za racunanje duzine liste.
;Iterativan proces.
(define (length-1 arr)
  (define (l-iter items counter)
    (if (null? items)
        counter
        (l-iter (cdr items) (+ counter 1))))
  (l-iter arr 0))

;Rekurzivan proces.
(define (length-2 arr)
  (if (null? arr)
      0
      (+ 1 (length-2 (cdr arr)))))

;Drugi nacin jeste da pristupamo preko indeksa elementima lista.
;Pocesvi od kraja liste redom ubacujemo elemente u novu listu.
;Iterativan proces.
(define (reverse-2 arr)
  (define (reverse-iter k n new-list) ;Od k-tog do n-tog indeksa, dodaje elemente iz pocetne liste u novu listu.
    (if (> k n)
        new-list
        (reverse-iter (+ k 1) n (cons (list-ref arr k) new-list))))
  (reverse-iter 0 (- (length-1 arr) 1) null))

;Treci nacin je da imamo dve liste: staru i novu.
;Stara lista predstavlja listu ciji je redosled elemenata potrebno obrnuti.
;Nova lista predstavlja obrnutu staru listu.
;Proces u svakom koraku uzima prvi element stare liste i spaja ga na pocetak nove liste, za sledeci korak prosledjuje staru listu bez prvog elementa i novu listu.
;Proces se zaustavlja kada prva lista nema vise elemenata.
;Proces je iterativan.
(define (reverse-3 items)
  (define (reverse-iter old-list new-list)
    (if (null? old-list)
        new-list
        (reverse-iter (cdr old-list) (cons (car old-list) new-list))))
  (reverse-iter items null))