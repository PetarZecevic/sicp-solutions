#lang racket

;Akumulator, akumulira po zadatoj operaciji vrednosti liste na pocetnu vrednost.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

;Redefinisanje brojanja listova kao akumulacije.
;Ako naidje na list, onda uvecava brojac za 1, ako naidje na podstablo onda
; uvecava brojac za broj listova u podstablu.
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (item)
                     (if (pair? item)
                         (count-leaves item)
                         1)) t)))

(define test-equal (lambda (x y) (= x y)))
(test-equal (count-leaves (list 1 1 (list 2 2 (list 2))))
            5)
(test-equal (count-leaves null)
            0)
(test-equal (count-leaves (list 1))
            1)
(test-equal (count-leaves (list 1 2 3 4 5))
            5)
(test-equal (count-leaves (list (list (list (list 1)))))
            1)