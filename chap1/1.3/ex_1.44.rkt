#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

;Primenjuje proceduru f na argument x, n-puta.
;Iterativan proces, logaritamske kompleksnosti.
(define (repeated f n)
  (define (even? n) (= (remainder n 2) 0))
  (define (rep proc times)
    (cond ((<= times 1) proc)
          ((even? n) (rep (compose proc proc) (/ times 2)))
          (else (rep (compose f proc) (- times 1)))))
  (rep f n))

;Glacanje funkcije, znacajno kod obrade signala.
(define dx 0.01)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

;n puta izglacana funkcija.
(define (n-fold f n)
  ((repeated smooth n) f))