#lang racket

(define (enumerate-interval start end)
    (if (> start end)
        null
        (cons start
              (enumerate-interval (+ start 1) end))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) sequence)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (prime? n)
  (define (prime-iter k)
    (cond ((> (* k k) n) #t)
          ((= (remainder n k) 0) #f)
          (else (prime-iter (+ k 2)))))
  (if (= (remainder n 2) 0)
      #f
      (prime-iter 3)))

;Kombinacija akumuliranja i mapiranja.
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list j i))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(prime-sum-pairs 8)