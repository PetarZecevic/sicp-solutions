#lang racket

;Genericne operacije.
(define (search sym symbols) ;trazi da li se dati simbol nalazi u listi simbola.
  (cond ((null? symbols) false)
        ((and (not (pair? (car symbols))) (eq? (car symbols) sym)) true)
        (else (search sym (cdr symbols)))))

(define (one-item? items)
  (null? (cdr items)))

;Koristi se kod ugnjezdenog izraza, trazi podizraz koji nije ugnjezden.
; ugnjezden - '((x + y))
; nije ugnjezden - ' ((x + y) * z)
(define (center exp)
  (if (and (pair? exp) (one-item? exp))
      (center (car exp))
      exp))
  
(define (left-part exp op)
  (cond ((or (null? exp) (eq? (car exp) op)) null)
        (else (if (pair? (car exp))
                  (append (list (car exp)) (left-part (cdr exp) op))
                  (cons (car exp) (left-part (cdr exp) op))))))
(define (right-part exp op)
  (cond ((null? exp) exp)
        ((eq? (car exp) op) (cdr exp))
        (else (right-part (cdr exp) op))))

;Derivation.
(define (variable? e)
  (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (make-exponentiation base exponent) ;n
  (cond ((=number? exponent 0) 1)
        ((or (=number? base 1) (=number? exponent 1)) base)
        (else (list base '** exponent))))

(define (sum? e) (search '+ (center e)))
(define (product? e) (search '* (center e)))
(define (exponentiation? e) (search '** (center e)))

;Levi i desni deo konkretnih operacija.
(define (addend s) (center (left-part (center s) '+)))
(define (augend s) (center (right-part (center s) '+)))
(define (multiplier p) (center (left-part (center p) '*)))
(define (multiplicand p) (center (right-part (center p) '*)))
(define (base e) (center (left-part (center e) '**)))
(define (exponent e) ;mora biti broj.
  (let ((n (center (right-part (center e) '**))))
    (if (number? n)
        n
        null)))

;Izvod izraza exp po promenljivoj var.
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (multiplicand exp)
                                 (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))