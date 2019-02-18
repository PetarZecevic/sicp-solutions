#lang racket

;Akumulator, akumulira po zadatoj operaciji vrednosti liste na pocetnu vrednost.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

;Verzija koja radi nad listom lista,sve podliste su iste duzine i kombinuju se korespodentni elementi i
;popunjavaju novu listu, koja se vraca kao povratna vrednost.
(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op initial (map (lambda (item) (car item)) seqs))
            (accumulate-n op initial (map (lambda (item) (cdr item)) seqs)))))

;Operacije koje rade nad matricama(lista lista, kod koje su sve podliste jednake duzine).
;Posto su podliste stabla jednake duzine mozemo da primenimo proceduru accumulate-n.

;Mnozenje vektora.
(define (dot-product v w)
  (accumulate + 0 (map * v w))) ;Koristi se produzeni oblik map primitivne operacije, gde kombinuje vise lista iste duzine u jednu.
;Alternativa za dot-product.
(define (dot-product-1 v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

;Mnozenje matrice i vektora.
(define (matrix-*-vector m v)
  (map (lambda (m-vector) (dot-product m-vector v)) m))

;Transponovanje matrice.
(define (transpose m)
  (accumulate-n cons
                null
                m))

;Mnozenje matrice sa matricom.
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-vector)
           (matrix-*-vector cols m-vector)) m)))

;Procedura za testiranje jednakosti, brojeva, lista i stabala.
(define test-equal (lambda (x y)
                     (cond ((and (pair? x) (pair? y)) (and (test-equal (car x) (car y))
                                                           (test-equal (cdr x) (cdr y))))
                           ((xor (pair? x) (pair? y)) #f)
                           ((xor (null? x) (null? y)) #f)
                           ((and (null? x) (null? y)) #t)
                           (else (= x y)))))