#lang racket

;Duz koja se nalazi izmedju datih tacaka.
(define (make-segment p1 p2) (cons p1 p2))
;Pocetna tacka duzi.
(define (start-segment segment) (car segment))
;Krajnja tacka duzi.
(define (end-segment segment) (cdr segment))

;Tacka.
(define (make-point x y) (cons x y))
;X-koordinata tacke.
(define (x-point p) (car p))
;Y-koordinata tacke.
(define (y-point p) (cdr p))

;Srednja tacka duzi.
(define (midpoint-segment segment)
  (define (average x y) (/ (+ x y) 2))
  (let ((x1 (x-point (start-segment segment)))
        (x2 (x-point (end-segment segment)))
        (y1 (y-point (start-segment segment)))
        (y2 (y-point (end-segment segment))))
    (make-point (average x1 x2)
           (average y1 y2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))