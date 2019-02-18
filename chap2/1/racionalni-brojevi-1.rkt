#lang racket

;Vazan princip konstruktora i selektora kod operacija nad datim podatkom.

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;Racionalni broj definisemo kao par, numerator i denumerator.
;Pri konstrukciji broja koristimo najveci zajednicki delilac, da bismo redukovali brojilac i imenilac.
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g)))) ;cons primitivni operator za konstrukciju jednog para.
;Numerator.
(define (numer x) (car x)) ;Primitivna operacija za izdvajanje prvog dela para.
;Denominator
(define (denom x) (cdr x)) ;Primitivna operacija za izdvajanje drugog dela para.

;Aritmeticke operacije nad racionalnim brojevima.
;Sabiranje.
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
;Oduzimanje.
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
;Mnozenje.
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
;Deljenje.
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))
;Jednakost.
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))