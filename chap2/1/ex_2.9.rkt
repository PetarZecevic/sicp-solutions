#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;Oduzimanje intervala.
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;Sabiranje dva intervala.
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;Mnozenje dva intervala.
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;Deljenje dva intervala.
;Mozemo predstaviti preko reciprocnog mnozenja.
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;Treba pokazati da su sabiranje i oduzimanje intervala funkcije koje zavise od duzine intervala.
(define (width interval) (/ (abs (- (upper-bound interval)
                               (lower-bound interval)))
                            2))
;Pretpostavka je da ce nakon sabiranja dva intervala rezultujuca sirina intervala,
;biti jednaka zbiru pojedinih sirina intervala koji se sabiraju.
(define (test-add interval-1 interval-2)
  (= (+ (width interval-1) (width interval-2))
     (width (add-interval interval-1 interval-2))))

;Pretpostavka je da ce nakon oduzimanja dva intervala, sirina rezultujuceg intervala
;biti jednaka razlici sirina intervala koji se oduzimaju.
(define (test-sub interval-1 interval-2)
  (= (+ (width interval-1) (width interval-2))
     (width (sub-interval interval-1 interval-2))))

;Probni intervali.
(define i-1 (make-interval -1 1))
(define i-2 (make-interval 0 4))

;Testiranje.
(display "Sirina prvog intervala: ")
(display (width i-1))
(newline)
(display "Sirina drugog intervala: ")
(display (width i-2))
(newline)

(display "sirina(i-1) + sirina(i-2) = sirina(i-1 + i-2)? ")
(display (test-add i-1 i-2))
(newline)

(display "sirina(i-1) + sirina(i-2) = sirina(i-1 - i-2)? ")
(display (test-sub i-1 i-2))