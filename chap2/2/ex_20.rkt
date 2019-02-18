#lang racket

;Procedura koja sabira proizvoljan broj argumenata.
;Obavezni argumenti su sa leve strane tacke a proizvoljni argumenti sa desne strane tacke.
;Proizvoljni parametri se predstavljaju u vidu liste.
(define (new-plus x y . z)
  (define (plus-iter result numbers)
    (if (null? numbers)
        result
        (plus-iter (+ result (car numbers)) (cdr numbers))))
  (plus-iter (+ x y) z))