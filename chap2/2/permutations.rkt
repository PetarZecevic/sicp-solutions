#lang racket

;Kreni kroz listu, i rekonstruisi novu listu.
;Kada naidjes na element koji treba da se izbaci, nemoj ga ubaciti u novi listu vec predji na sledeci element u listi.
(define (remove item items)
  (cond ((null? items) items)
        ((= (car items) item) (remove item (cdr items)))
        (else (cons (car items)
                    (remove item (cdr items))))))

;Remove realizovan preko filtera.
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence)) (cons (car sequence)
                                          (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (remove-1 item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

;Plan for generating permutations of a set S:
;For each item x in S, recursively generate the
;sequence of permutations of S - x, and adjoin x to the front of each one. This yields, for each x in S,
;the sequence of permutations of S that begin with x. Combining these sequences for all x gives all the
;permutations of S.
(define (permutations s)
  (if (null? s)
      (list null) ;Ne moze null jer map ne prolazi kroz null, a kroz (list null), ce proci i pokupiti null.
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p)) (permutations (remove x s))))
               s)))

(permutations (list 1 2 3))
(permutations (list 1 2))
(permutations (list 1))