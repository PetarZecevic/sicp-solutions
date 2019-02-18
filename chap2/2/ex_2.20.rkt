#lang racket

;Procedura koja vraca listu svih brojeva koji su iste parnosti kao prvi broj.
;Dva broja su iste parnosti ako je njihova razlika paran broj.
(define (same-parity x . z)
  (define (even? n) (= (remainder n 2) 0))
  (define (append list-1 list-2)
    (if (null? list-1)
        list-2
        (cons (car list-1) (append (cdr list-1) list-2))))
  (define (parity-iter numbers new-list)
    (cond ((null? numbers) new-list)
          ((even? (abs (- x (car numbers)))) (parity-iter (cdr numbers)
                                                          (append new-list (list (car numbers)))))
          (else (parity-iter (cdr numbers)
                             new-list))))
  (parity-iter z (list x)))

;Optimalnija verzija same-parity procedure koja ne koristi append proceduru.
(define (same-parity-opt x . z)
  (define (reverse-opt numbers) ;Obrtanje liste je uvedeno zbog lepseg ispisa.
    (define (r-iter r-list n-list) ;r-list je obrnuta lista dok je n-list pocetna(normalna) lista.
      (if (null? n-list)
          r-list
          (r-iter (cons (car n-list) r-list) (cdr n-list))))
    (r-iter (list (car numbers)) (cdr numbers)))
  (define (sp-iter sp-list numbers)
    (cond ((null? numbers) (reverse-opt sp-list)) ;Lista koja sadrzi x i ostale brojeve koji su iste parnosti kao x.
        ((even? (abs (- (car numbers) x))) (sp-iter (cons (car numbers) sp-list) (cdr numbers)))
        (else (sp-iter sp-list (cdr numbers)))))
  (sp-iter (list x) z))
