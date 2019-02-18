#lang racket

(define (enumerate-interval start end)
  (if (> start end)
      null
      (cons start (enumerate-interval (+ start 1) end))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append
              null
              (map proc seq)))

;Jedinstveni parovi brojeva (i,j) , tako da je 1 <= j < i <= n. 
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j)) (enumerate-interval 1 (- i 1))))(enumerate-interval 1 n)))

(define (filter predicate sequence)
  (cond ((null? sequence) sequence)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;Jedinstvene trojke preko ugnjezdenih mapiranja.
(define (unique-triples n)
  (flatmap (lambda (k)
           (flatmap (lambda (i)
                    (map (lambda (j)
                           (list k i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 (- k 1)))) (enumerate-interval 1 n)))

;Jedinstvene trojke brojeva (k,i,j) tako da je 1 <= j < i < k <= n and i + j + k = s.
(define (ordered-triples n s)
  (filter (lambda (triple)
            (= (accumulate + 0 triple) s)) (unique-triples n)))