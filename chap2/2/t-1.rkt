#lang racket

;General acumulator.
(define (acumulator start end condition next operator f result)
  (if (condition start end)
      result
      (acumulator (next start) end condition next operator f (operator (f start) result))))

;Filter list.
(define (filter items criteria)
  (if (null? items)
      null
      (let ((first (car items))
            (next (cdr items)))
        (if (criteria first)
            (cons first (filter next criteria))
            (filter next criteria)))))

;Formiranje liste od listova stabla.
(define (form-list tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (form-list (car tree))
                      (form-list (cdr tree))))))
