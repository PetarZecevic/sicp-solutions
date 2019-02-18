#lang racket

(define (f g) (g 2))
; Poziv (f f) izaziva gresku, jer je u procesu evalucije doslo do toga da je 2 uzet kao operator.