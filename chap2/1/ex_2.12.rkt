#lang racket

(define (make-interval a b)
  (if (> a b) ;Osigurava donju i gornju granicu.
      (cons b a)
      (cons a b)))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;Sirina intervala.
(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))

;Mnozenje dva intervala.
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

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

(define int-1 (make-interval 1 2))
(display "Centar i procenat prvog: ")
(display (center int-1))
(display " - ")
(display (percent int-1))
(newline)

(define int-2 (make-interval 2 3))
(display "Centar i procenat drugog: ")
(display (center int-2))
(display " - ")
(display (percent int-2))
(newline)

