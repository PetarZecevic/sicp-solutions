#lang racket

;Genericka procedura ciji su specijalni slucajevi: sum and product procedure.
;Akumulator, akumulise vrednosti.
;Term transformise tekucu vrednost.
;Kombinator povezuju tekucu sa prethodnim vrednostima.
;Null-value se vraca u slucaju kada se predje odredjeni opseg.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))

;Akumulator realizovan kao iterativan proces.
(define (accumulate-it combiner null-value term a next b)
  (define (iter a b res)
    (if (< a b)
        res
        (iter (next a) b (combiner (term a) res))))
  (iter a b null-value))

(define (sum a b)
  (define (identity x) x)
  (define (next x) (+ x 1))
  (accumulate + 0 identity a next b))