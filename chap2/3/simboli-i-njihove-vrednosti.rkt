#lang racket

;Znak ' stavljamo na pocetku kada radimo sa simbolickim podatkom.
(define a 5)
(define b 10)
(list a b)
(list 'a 'b)
(list 'a b)
(list a 'b)

(car '(a b c))
(cdr '(a b c))

(quote a)
(quote (a b c))
(car (quote (a b c)))
;Konstruisanje izraza (car (quote (a b c))).
(list 'car (list 'quote '(a b c)))

;Potraga za simibolom unutar liste, ako ga nadje vraca podlistu koja pocinje tim simbolom.
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(banana orange pear))
(memq 'milan '(jovan aleksa milan rade))
(memq '5 '(1 2 3 4 5))