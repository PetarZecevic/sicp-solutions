#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;Oduzimanje intervala.
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;Sabiranje dva intervala.
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;Mnozenje dva intervala.
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;Deljenje dva intervala.
;Mozemo predstaviti preko reciprocnog mnozenja.
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;Sirina intervala.
(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))


;Interval definisan kao centralna vrednost i procenat oko centralne vrednosti.
(define (make-center-percent center percent)
  (let ((tolerance (* percent (/ center 100.0))))
  (make-interval (- center tolerance) (+ center tolerance))))

;Centar intervala.
(define (center interval)
  (/ (+ (lower-bound interval)
        (upper-bound interval))
     2.0))

;Procenat odstupanja od centra, procenat od vrednosti centra.
(define (percent interval)
  (* (/ (width interval) (center interval))
     100.0))

(define r1 (make-interval 1.0 1.5))
(define r2 (make-interval 2.0 2.5))