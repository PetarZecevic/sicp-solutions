#lang racket

;Procedura koja vraca listu koja sadrzi poslednji element liste.
;Zove se last-pair zato sto je poslednji element liste zapravao par element-null.
;Radi sa nepraznim listama.
(define (last-pair arr)
  (if (null? (cdr arr)) ;Lista koja sadrzi jedan element
      arr
      (last-pair (cdr arr))))