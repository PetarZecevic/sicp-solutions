#lang racket
;;Optimizovan Njutnov metod sa promenjenom procedurom good-enough?,
;koja prati promeni guess parametra iz iteracije u iteraciju i kada,
;promena postane dovoljno mala zakljucuje se da se stiglo blizu korektne vrednosti.
(define (abs x) ;Apsolutna vrednost.
  (if (< x 0)
      (- x)
      x))

(define (average x y) ;Srednja vrednost. 
  (/ (+ x y) 2))

(define (<= x y) ;Relacija manje-jednako.
  (not (> x y)))

(define (good-enough? prev-guess next-guess) ;Procena bliskosti resenju.
  (<= (abs (- prev-guess next-guess)) ;Poredjenje prethodne i naredne iteracije.
      0.001)) ;Preciznost.

(define (improve guess x) ;Poboljsanje resenja kroz iteracije.
  (average guess (/ x guess)))

(define (square-iter guess x) ;Aproksimativni Njutnov algoritam za nule funkcije, u nasem slucaju kvadratne funkcije.
  (if (good-enough? guess (improve guess x))
      guess
      (square-iter (improve guess x) x)))

(define (sqrt x)
  (square-iter 1.0 x))