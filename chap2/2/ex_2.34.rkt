#lang racket

;Akumulator, akumulira po zadatoj operaciji vrednosti liste na pocetnu vrednost.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

;Hornerovo pravilo za racunanje vrednosti polinoma u tacki x, koeficijenti polinoma su a0, a1, a2, ... , an.
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; 1 + x + x^2 + x^4
(define test-1 (= (horner-eval 1 (list 1 1 1 1))
                  4))
; 2 + 3*x + 4*x^2
(define test-2 (= (horner-eval 1 (list 2 3 4))
                9))
; x + 2*x^3
(define test-3 (= (horner-eval 3 (list 0 1 0 2))
                  57))

(newline)
(display "Uspesnost prvog testa: ")
(display test-1)
(newline)
(display "Uspesnost drugog testa: ")
(display test-2)
(newline)
(display "Uspesnost treceg testa: ")
(display test-3)