#lang racket

;Racunanje eksponenta broja i modula tog broja.
;Logaritamska kompleksnost.
;Rekurzivan proces.
(define (expmod base exponent moduo)
  (define (even? n) (= (remainder n 2) 0))
  (define (square n) (* n n))
  (cond ((= exponent 1) (remainder base moduo))
        ((even? exponent) (remainder (square (expmod base (/ exponent 2) moduo)) moduo))
        (else (remainder (* base (expmod base (- exponent 1) moduo)) moduo))))

;Fermatov metod za utvrdjivanje prostosti broja.
;a^n <=> a (mod n), za svako a < n, ako je n prost broj.
(define (fermat-test n times)  ;Times se odnosi na to koliko puta ponavljamo test, da bi smo bili sigurni da je broj prost, posto se radi o verovatnosnom algoritmu.
  (define (test a) ;Jedan test.
    (= (expmod a n n) a))
  (cond ((= times 0) true)
        ((test (+ 1 (random (- n 1)))) (fermat-test n (- times 1)))
        (else false)))


(define (prime-time-test n)
  (define (start-test-prime start-time)
    (cond ((fermat-test n 4) (report-prime (- (current-milliseconds) start-time)))))
  (define (report-prime elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))
  (start-test-prime (current-milliseconds)))

;Trazenje prostih brojeva uspomoc Fermatovog metoda.
(define (search-for-primes lower-bound how-many)
  (define (even? n) (= (remainder n 2) 0))
  (define (first-k-primes num k found)
    (cond ((< found k) (prime-time-test num) (first-k-primes (+ num 2) k (if (fermat-test num 4)
                                                                             (+ found 1)
                                                                             found)))))
  (define (adjust-first n) ;Optimizacija kako bi se automatski prebacivalo na sledeci neparni broj.
    (if (even? n)
        (+ n 1)
        n))
  (first-k-primes (adjust-first lower-bound) how-many 0))