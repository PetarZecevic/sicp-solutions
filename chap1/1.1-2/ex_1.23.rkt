#lang racket

(define (square x) (* x x)) ;Kvadriranje.
(define (divisible? a b) (= (remainder a b) 0)) ;Proverava da li je a deljivo sa b.

;Optimizacija procedure smallest-divisor.
;Poenta je da nije potrebno uzimati dalje parne brojeve, ako broj nije deljiv sa 2.
;Tada se uzimaju samo naredni neparni brojevi kao potencijalni delioci broja.
(define (smallest-divisor num)
    (define (find-divisor n k)
      (cond ((> (square k) n) n) ;Slucaj kada je broj prost, tada je deljiv samo sa samim sobom.
            ((divisible? n k) k) ;Pronasao prvog delioca
            (else (find-divisor n (+ k 2))))) ;Iterira dok ne pronadje delioca.
    (define (adjust-first n)
      (if (divisible? n 2)
          2 ;Ako je broj deljiv sa 2, tada ce biti tacan drugi predikat u cond operaciji i zavrsice se procedura.
          3)) ;Ako broj nije deljiv sa 2, krece se od 3 i testiraju se samo neparni brojevi.
    (find-divisor num (adjust-first num)))

;Klasican metod za odredjivanje prostog broja.
(define (prime? n)
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