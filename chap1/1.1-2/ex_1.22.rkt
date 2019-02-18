#lang racket

;Procedura za utvrdjivanje da li je n prost broj.
(define (prime? n)
  (define (smallest-divisor num)
    (define (square x) (* x x)) ;Kvadriranje.
    (define (divisible? a b) (= (remainder a b) 0)) ;Proverava da li je a deljivo sa b.
    (define (find-divisor n k)
      (cond ((> (square k) n) n) ;Slucaj kada je broj prost, tada je deljiv samo sa samim sobom.
            ((divisible? n k) k) ;Pronasao prvog delioca
            (else (find-divisor n (+ k 1))))) ;Iterira dok ne pronadje delioca.
    (find-divisor num 2))
  (= (smallest-divisor n) n))

;Racuna vreme izvrsenja procedure prime? u milisekundama.
(define (prime-time-test n)
  (define (start-test-prime start-time)
    (cond ((prime? n) (report-prime (- (current-milliseconds) start-time)))))
  (define (report-prime elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))
  (start-test-prime (current-milliseconds)))

;Trazi odredjeni broj najmanjih prostih brojeva, pocesvi od prvog broja.
;Testiraju se samo neparni brojevi, znamo da su neparni brojevi udaljeni za dva mesta.
;Jer ce donja granica biti sigurno veca od 2.
(define (search-for-primes lower-bound how-many)
  (define (even? n) (= (remainder n 2) 0))
  (define (first-k-primes num k found)
    (cond ((< found k) (prime-time-test num) (first-k-primes (+ num 2) k (if (prime? num)
                                                                             (+ found 1)
                                                                             found)))))
  (define (adjust-first n) ;Optimizacija kako bi se automatski prebacivalo na sledeci neparni broj.
    (if (even? n)
        (+ n 1)
        n))
  (first-k-primes (adjust-first lower-bound) how-many 0))