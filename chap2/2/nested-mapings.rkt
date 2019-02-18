#lang racket

;Za dato n trazi liste (i,j,i+j), tako da vazi (1 <= j < i <= n) && (i+j je prost broj)

;Prvi korak je pronaci sve parove (i,j).
;Drugi korak je filtrirati parove (i,j) tako da je i+j prost broj.
;Treci korak je formirati liste (i,j,i+j).

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

;Formiranje parova (i,j).
(define (form-pairs n)
  (accumulate append
              null
              (map (lambda (i)
                     (map (lambda (j)
                            (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))))
;Filter.
(define (filter predicate sequence)
  (cond ((null? sequence) sequence)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;Provera parnosti broja, tacnije zbira i+j.
(define (prime? n)
  (define (prime-iter k)
    (cond ((> (* k k) n) #t)
          ((= (remainder n k) 0) #f)
          (else (prime-iter (+ k 2)))))
  (if (= (remainder n 2) 0)
      #f
      (prime-iter 3)))

(define prime-condition (lambda (pair)
                        (prime? (+ (car pair) (cadr pair)))))

;Formiranje lista sa tri clana (i,j,i+j).
(define (form-trios pairs)
  (map (lambda (pair)
         (append pair (list (+ (car pair) (cadr pair))))) pairs))

;Nadovezivanje koraka.
;Pravljenje blok-dijagrama.
(define (prime-sum-pairs n)
  (form-trios
   (filter prime-condition
           (form-pairs n))))