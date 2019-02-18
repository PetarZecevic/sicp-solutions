#lang racket

;Vraca n-ti element liste, indeksiranje liste pocinje od 0.
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;Vraca duzinu liste, koristi primitivni operator null? koji vraca #t ako je lista prazna, u suprotnom #f.
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;Konkatenacija lista.
;Dve moguce interpretacije:
;- Na kraj prve liste se dodaje druga lista.
;- Na pocetak druge liste se dodaje prva lista.
;Druga interpretacija se koristi zbog prirode operacije cons koja dodaje element na pocetak liste.
(define (append-1 list-1 list-2)
  (if (null? list-1)
      list-2
      (cons (car list-1) (append-1 (cdr list-1) list-2))))