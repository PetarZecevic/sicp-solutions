#lang racket

; Racunanje frakcije:
;    N1
; -------
;        N2
; D1 + ------
;     ..  Nk
;       + --
;         Dk

; n i d su procedure koje vracaju neki broj kada im se prosledi indeks i.
;Rekurzivan proces.
;Top-down pristup.
(define (cont-frac n d k)
  (define (frac i)
    (let ((N (n i))
        (D (d i)))
    (if (> i k)
        0
        (/ N (+ D (frac (+ i 1)))))))
  (frac 1))

;Iterativan proces.
;Down-top pristup.
;Krece od: Nk-1 / (Dk-1 + (Nk/Dk))
;Prvi rezultat je (Nk/Dk), kasnije se rezultat koristi rekurivno u ostalim proracunima.
(define (cont-frac-iter n d k)
  (define (iter result k)
    (let ((s (/ (n k) (+ (d k) result))))
      (if (= k 0)
          result
         (iter s
              (- k 1)))))
  (iter (/ (n k) (d k)) (- k 1)))
