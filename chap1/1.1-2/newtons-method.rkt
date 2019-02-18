#lang racket

;Trazimo x tako da je f(x) = x, f(f(x)) = x, f(f(f(x))) = x, ...
(define tolerance 0.001)
(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try a)
    (if (close-enough? (f a) a)
        a
        (try (f a))))
  (try guess))

;Njutnov metod za pronalazenje nule funckije g(x) = 0, uspomoc fiksne tacke funkcije
; f(x) = x - g(x)/Dg(x), gde je Dg(x) = (g(x+dx) - g(x))/dx izvod funkcije g(x).

;Aproksimaciona konstanta za izvod.
(define dx 0.000001)

;Procedura koja vraca izvod bilo koje funkcije.
(define (derivative g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

;Transformise g u f kao sto je gore navedeno.
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((derivative g) x)))))

;Racuna fiksnu g(x) = 0, na osnovu Njutnove transformacije.
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;Procedura za racunanje korena nekog broja realizovana preko Njutnove metode.
(define (sqrt-n x)
  (newtons-method (lambda (y) (- (* y y) x)) 1.0))