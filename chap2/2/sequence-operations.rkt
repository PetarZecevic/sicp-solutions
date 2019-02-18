#lang racket

;Formiramo graf toka signala, gde je signal predstavljen listom vrednosti.

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

;Enumerisanje listova stabla.
(define (enumerate-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;Enumerisanje intervala brojeva.
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;Suma kvadrata neparnih listova stabla.
(define (sum-odd-squares tree)
  (acumulator +
             0
             (map (lambda (x) (* x x))
                  (filter odd?
                          (enumerate-tree tree)))))

;Lista parnih fibonacijevih brojeva manjih od n.
(define (even-fibs n)
  (define (fib n)
    (if (<= n 1)
        1
        (+ (fib (- n 2))
           (fib (- n 1)))))
  (acumulator cons
              null
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))