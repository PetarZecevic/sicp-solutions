#lang racket

;Proverava da li je ostalo jos novcica koji mogu da ucestvuju u razmeni.
(define (no-more? items)
  (null? items))

;Eliminise prvi novcic iz liste novcica za vracanje kusura.
(define (except-first-denomination coins)
  (cdr coins))

;Uzima vrednost prvog novcica.
(define (first-denomination coins)
  (car coins))

;Procedura koja racuna na koliko nacina moze da se vrati kusur, sa datim novcicima.
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
;Redosled u kom se proslede novcici ne utice na rezultat racunanja.