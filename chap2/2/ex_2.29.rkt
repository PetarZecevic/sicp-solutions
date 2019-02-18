#lang racket

;Binarni modul koji se sadrzi od 2 grane, svaka grana je konopac odredjene duzine na cijem kraju se moze nalaziti materijal odredjene tezine ili drugi binarni modul.

;Modul.
(define (make-mobile left right)
  (list left right))
;Grana.
(define (make-branch length structure)
  (list length structure))
;a
;Vraca levu granu.
(define (left-branch mobile)
  (car mobile))
;Vraca desnu granu.
(define (right-branch mobile)
  (cadr mobile))
;Duzina grane.
(define (branch-length branch)
  (car branch))
;Struktura koju "nosi" grana.
(define (branch-structure branch)
  (cadr branch))
;b
;Celokupna tezina koju "nosi" modul.
(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile ;Ako nema grane onda se radi o materijalu odredjene tezine.
      (+ (total-weight (branch-structure (left-branch mobile))) ;Ako modul ima grane saberi tezine koje nose grane.
         (total-weight (branch-structure (right-branch mobile))))))

(define m-1 (make-mobile (make-branch 1 5)
                         (make-branch 2 (make-mobile (make-branch 3 6)
                                                     (make-branch 5 5)))))
(define m-2 (make-mobile (make-branch 1 (make-mobile (make-branch 2 (make-mobile (make-branch 1 2)
                                                                                 (make-branch 1 2)))
                                                     (make-branch 2 2)))
                         (make-branch 1 10)))
(total-weight m-2)
(total-weight m-1)

;c
;Testiranje balansiranosti modula.
;Modul je balansiran ako je (duzina leve grane) * (tezina koju leva grana nosi) = (duzina desne grane) * (tezina koju desna grana nosi)
;Ako modul sadrzi podmodule i oni moraju biti izbalansirani.
(define (balanced mobile)
  (let ((left-torque (* (branch-length (left-branch mobile))
                        (total-weight (branch-structure (left-branch mobile)))))
        (right-torque (* (branch-length (right-branch mobile))
                         (total-weight (branch-structure (right-branch mobile)))))
        (left-struct (branch-structure (left-branch mobile))) ;Struktura koju nosi leva grana.
        (right-struct (branch-structure (right-branch mobile)))) ;Struktura koju nosi desna grana.
    (and (= left-torque right-torque)
         (if (pair? left-struct) (balanced left-struct) #t) ;Ako je struktura modul, proveri da li je modul izbalansiran.
         (if (pair? right-struct) (balanced right-struct) #t))))

(define m-3 (make-mobile (make-branch 2 3) (make-branch 3 2)))
(define m-4 (make-mobile (make-branch 2 10)
                         (make-branch 2 (make-mobile (make-branch 2 5)
                                                     (make-branch 2 5)))))
(balanced m-1)
(balanced m-2)
(balanced m-3)
(balanced m-4)

;d
;Postavlja se pitanje na koliko mesta moramo da menjamo program uvodjenjem sledecih izmena:
;(define (make-mobile left right)
;  (cons left right))
;(define (make-branch length structure)
;  (cons length structure))
;
;Posto su razgraniceni nivoi apstrakcije: implementacije i koriscenja metoda, potrebno napraviti izmene samo u sledecim procedurama:
;(define (right-branch module)
;  (cdr module))
;(define (branch-structure branch)
;  (cdr branch))