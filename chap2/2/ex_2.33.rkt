#lang racket

;Filtar, uzima vrednosti iz liste po zadatom kriterijumu i stavlja ih u novu listu.
(define (filter predicate sequence)
  (cond ((null? sequence) sequence)
        ((predicate (car sequence)) (cons (car sequence)
                                          (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;Akumulator, akumulira po zadatoj operaciji vrednosti liste na pocetnu vrednost.
(define (acumulator op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (acumulator op
                      initial
                      (cdr sequence)))))

;Primitivne operacije za manipulaciju nad listama, opisane preko sekvencijalnih operacija.
(define (map p sequence)
  (acumulator (lambda (x y) (cons (p x) y))
              null
              sequence))

(define (append seq1 seq2)
  (acumulator cons
              seq2
              seq1))

(define (length sequence)
  (acumulator (lambda (x y) (+ 1 y))
              0
              sequence))